open Rresult
open Bos
open Astring
open R.Infix
module OC = OS.Cmd

let run ?env c =
  Logs.info (fun l -> l "exec: %a" Cmd.pp c);
  OS.Cmd.run ?env c

let run_out ?env c =
  Logs.info (fun l -> l "exec: %a" Cmd.pp c);
  OS.Cmd.run_out ?env c

(** Docker *)
module Docker = struct
  let bin = Cmd.(v "docker")
  let info = Cmd.(bin % "info")
  let exists () =
    run_out info |> OC.out_string |> OC.success |> R.is_ok |>
    function
    | true -> Logs.info (fun l -> l "Docker is running"); true
    | false -> Logs.err (fun l -> l "Docker not running"); false

  let build ?(pull=true) ?(cache=true) ?dockerfile ?tag path =
    let open Cmd in
    let cache = if cache then empty else v "--no-cache" in
    let pull = if pull then v "--pull" else empty in
    let dfile = match dockerfile with None -> empty | Some d -> v "-f" % p d in
    let tag = match tag with None -> empty | Some t -> v "-t" % t in
    bin % "build" %% tag %% cache %% pull %% dfile  % p path

  (* Find the image id that we just built *)
  let build_id log =
    match String.find ~rev:true (fun s -> s = '\n') log with
    | None -> R.error "Failed build"
    | Some first ->
        String.with_range ~first log |> fun line ->
        Logs.debug (fun l -> l "last log line: %s" line);
        R.ok line 
end

(** Gnu Parallel *)
(* parallel --retries 1 --joblog ../logs/joblog.txt --bar --results ../logs docker build --pull --no-cache -f Dockerfile.{} -t opam-{} . ::: ${distros *)
module Parallel = struct
  let bin = Cmd.(v "parallel")
  let run ?retries ?results ?joblog cmd args =
    let open Cmd in
    let retries =
      match retries with
      | None -> empty
      | Some r -> v "--retries" % string_of_int r in
    let joblog =
      match joblog with
      | None -> empty
      | Some j -> v "--joblog" % p j in
    let results =
      match results with
      | None -> empty
      | Some r -> v "--results" % p r in
    bin % "--no-notice" %% retries %% joblog %% results %% cmd % ":::" %% args
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
