open Sexplib.Conv
open Rresult
open Bos
open Astring
open R.Infix
module OC = OS.Cmd

let rec iter fn l =
  match l with
  | hd::tl -> fn hd >>= fun () -> iter fn tl
  | [] -> Ok ()

let run_log ?env log_dir name cmd =
  OS.File.write Fpath.(log_dir / (name ^ ".cmd")) (Cmd.to_string cmd) >>= fun () ->
  let err = OS.Cmd.err_file Fpath.(log_dir / (name ^ ".err")) in
  OS.Cmd.run_out ?env ~err cmd |>
  OS.Cmd.out_file Fpath.(log_dir / (name ^ ".out")) >>= fun ((), (_,status)) ->
  let write_exit code = 
   OS.File.write Fpath.(log_dir / (name ^ ".exitcode")) (string_of_int code) in
  match status with
  |`Signaled n ->
     R.error_msg (Fmt.strf "Signal %d" n)
  |`Exited 0 -> write_exit 0 >>= fun () -> Ok ()
  |`Exited code -> write_exit code >>= fun () -> R.error_msg (Fmt.strf "exit code %d" code)

(** Docker *)
module Docker = struct
  let bin = Cmd.(v "docker")
  let info = Cmd.(bin % "info")

  let exists () =
    OS.Cmd.run_out info |> OS.Cmd.out_string |> R.is_ok |>
    function
    | true -> Logs.info (fun l -> l "Docker is running"); true
    | false -> Logs.err (fun l -> l "Docker not running"); false

  let build_cmd ?(squash=false) ?(pull=true) ?(cache=true) ?dockerfile ?tag path =
    let open Cmd in
    let cache = if cache then empty else v "--no-cache" in
    let pull = if pull then v "--pull" else empty in
    let squash = if squash then v "--squash" else empty in
    let dfile = match dockerfile with None -> empty | Some d -> v "-f" % p d in
    let tag = match tag with None -> empty | Some t -> v "-t" % t in
    bin % "build" %% tag %% cache %% pull %% squash %% dfile  % p path

  let push_cmd tag =
    Cmd.(bin % "push" % tag)

  (* Find the image id that we just built *)
  let build_id log =
    let rec find_id =
      function
      | hd::tl when String.is_prefix ~affix:"Successfully tagged " hd -> find_id tl
      | hd::tl when String.is_prefix ~affix:"Successfully built " hd -> begin
         match String.cut ~sep:"Successfully built " hd with
         | Some ("", id) -> R.ok id
         | Some _ -> R.error_msg "Unexpected internal error in build_id"
         | None -> R.error_msg "Malformed successfully built log"
      end
      | hd::tl -> R.error_msg "Unexpected lines at end of log"
      | [] -> R.error_msg "Unable to find container id in log" in
    OS.File.read_lines log >>= fun lines ->
    List.rev lines |> fun lines ->
    find_id lines

  let manifest_push_cli ~platforms ~template ~target =
    let platforms = String.concat ~sep:"," platforms in
    Cmd.(v "manifest-tool" % "push" % "from-args" % "--platforms" % platforms
         % "--template" % template % "--target" % target)

  let manifest_push_file file =
     Cmd.(v "manifest-tool" % "push" % "from-spec" % p file)
end

(** Gnu Parallel *)
module Parallel = struct

  module Joblog = struct
    type t = {
      arg: string;
      seq: int;
      host: string;
      start_time: float;
      run_time: float;
      send: int;
      receive: int;
      exit_code: int;
      signal: int;
      command: string;
      build_logfiles: (string * string) option;
    } [@@deriving sexp]

    let of_csv_row row =
      let find = Csv.Row.find row in
      let find_int field = find field |> int_of_string in
      let find_float field = find field |> float_of_string in
      { arg = ""; build_logfiles = None;
        seq = find_int "Seq";
        host = find "Host";
        start_time = find_float "Starttime";
        run_time = find_float "JobRuntime";
        send = find_int "Send";
        receive = find_int "Receive";
        exit_code = find_int "Exitval";
        signal = find_int "Signal";
        command = find "Command" }

    let v file =
      open_in (Fpath.to_string file) |>
      Csv.of_channel ~has_header:true ~separator:'\t' ~strip:true |>
      Csv.Rows.input_all |>
      List.map of_csv_row
  end

  type joblog = Joblog.t [@@deriving sexp]
  type t = joblog list [@@deriving sexp]
  let bin = Cmd.(v "parallel")

  let run_cmd ?delay ?retries ?results cmd args =
    let open Cmd in
    let args = of_list args in
    let retries =
      match retries with
      | None -> empty
      | Some r -> v "--retries" % string_of_int r in
    let delay =
      match delay with
      | None -> empty
      | Some d -> v "--delay" % (Fmt.strf "%.2f" d) in
    let joblog =
      match results with
      | None -> empty
      | Some r -> v "--joblog" % p Fpath.(r / "joblog.txt") in
    let results =
      match results with
      | None -> empty
      | Some r -> v "--results" % p r in
    bin % "--no-notice" %% retries %% joblog %% delay %% results %% cmd % ":::" %% args

  let run ?delay ?retries logs_dir label cmd args =
    let results = Some logs_dir in
    let t = run_cmd ?delay ?retries ?results cmd args in
    run_log logs_dir label t >>= fun _ ->
    match results with
    | None -> R.ok []
    | Some f ->
       Joblog.v Fpath.(f / "joblog.txt") |>
       List.map (fun j ->
         let arg = List.nth args (j.Joblog.seq - 1) in
         let build_logfiles =
           match results with
           | None -> None
           | Some d ->
               let path = Fmt.strf "%a/1/%s/" Fpath.pp d "TODO" in
               Some (path ^ "stdout", path ^ "stderr") in
         { j with arg; build_logfiles }) |> fun r ->
         let fails = List.filter (fun {Joblog.exit_code;_} -> exit_code <> 0) r in
         let is_ok = List.length fails = 0 in
         if is_ok then Ok r else begin
           let msg = Fmt.strf "Failed %d jobs: %s" (List.length fails)
            (Sexplib.Sexp.to_string_hum (sexp_of_t fails)) in
           R.error_msg msg
         end
end

(** Opam *)
module Opam = struct
  let bin = Cmd.(v "opam")

  let opam_env ~root ~jobs =
    OS.Env.current () >>= fun env ->
    String.Map.add "OPAMROOT" (Cmd.p root) env |>
    String.Map.add "OPAMYES" "1" |>
    String.Map.add "OPAMJOBS" (string_of_int jobs) |> fun env ->
    R.return env
end

module Mdlog = struct

  type cmd =
  | Parallel of {label:string; args:string list } [@@deriving sexp]
  
  type cmds = cmd list [@@deriving sexp]

  type t = {
    mutable cmds: cmds;
    logs_dir: Fpath.t;
    prefix: string;
    descr: string;
  }
  
  let init ~logs_dir ~prefix ~descr =
    { cmds=[]; logs_dir; prefix; descr }

  let run_parallel ?retries ?delay t label cmd args =
    (* TODO still log on failure *)
    let logs_dir = Fpath.(t.logs_dir) in
    Parallel.run ?retries ?delay logs_dir label cmd args >>= fun jobs ->
    t.cmds <- Parallel {label; args} :: t.cmds;
    Ok ()

  let output t =
    let cmds = List.rev t.cmds in
    Logs.info (fun l -> l "%s" (Sexplib.Sexp.to_string_hum (sexp_of_cmds cmds)));
    Ok ()
end


open Cmdliner
let setup_logs () =
  let setup_log style_renderer level =
    Fmt_tty.setup_std_outputs ?style_renderer ();
    Logs.set_level level;
    Logs.set_reporter (Logs_fmt.reporter ()) in
  let global_option_section = "COMMON OPTIONS" in
  Term.(const setup_log
    $ Fmt_cli.style_renderer ~docs:global_option_section ()
    $ Logs_cli.level ~docs:global_option_section ())
