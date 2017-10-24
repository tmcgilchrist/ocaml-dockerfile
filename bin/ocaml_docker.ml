(* generate ocaml docker containers *)
module L = Dockerfile_linux
module D = Dockerfile_distro
module C = Dockerfile_cmd

module Gen = struct
open Dockerfile
open Dockerfile_opam
(* Build the OPAM distributions from the OCaml base *)
let add_comment ?compiler_version tag =
  comment "OPAM for %s with %s" tag
  (match compiler_version with
      | None -> "system OCaml compiler"
      | Some v -> "local switch of OCaml " ^ v)

(* Apk based Dockerfile *)
let apk_opam2 ?(labels=[]) ~distro ~tag () =
  header distro tag @@
  label (("distro_style", "apk")::labels) @@
  L.Apk.install "build-base bzip2 git tar curl ca-certificates" @@
  install_opam_from_source ~install_wrappers:true ~branch:"master" () @@
  from ~tag distro @@
  copy ~from:"0" ~src:["/usr/local/bin/opam*"] ~dst:"/usr/bin" () @@
  L.Apk.install "build-base tar ca-certificates git rsync curl sudo" @@ 
  L.Apk.add_user ~sudo:true "opam" @@
  L.Git.init ()

(* Debian based Dockerfile *)
let apt_opam2 ?(labels=[]) ~distro ~tag () =
  header distro tag @@
  label (("distro_style", "apt")::labels) @@
  L.Apt.install "build-essential curl git" @@
  install_opam_from_source ~install_wrappers:true ~branch:"master" () @@
  from ~tag distro @@
  copy ~from:"0" ~src:["/usr/local/bin/opam*"] ~dst:"/usr/bin" () @@
  L.Apt.install "build-essential curl git rsync sudo unzip" @@
  L.Git.init ()

let gen_opam_for_distro ?labels d =
  match D.resolve_alias d with
  | `Alpine v ->
       let tag = match v with
         | `V3_3 -> "3.3" | `V3_4 -> "3.4"
         | `V3_5 -> "3.5" | `V3_6 -> "3.6"
         | `Latest -> assert false in
       Some (D.tag_of_distro d, (apk_opam2 ?labels ~distro:"alpine" ~tag ()))
  | `Debian v ->
       let tag = match v with
         | `V7 -> "7"
         | `V8 -> "8"
         | `V9 -> "9"
         | `Testing -> "testing"
         | `Unstable -> "unstable"
         | `Stable -> assert false in
       Some (D.tag_of_distro d, (apt_opam2 ?labels ~distro:"debian" ~tag ()))
  | `Ubuntu v ->
      let tag = match v with
        | `V12_04 -> "precise"
        | `V14_04 -> "trusty"
        | `V16_04 -> "xenial"
        | `V16_10 -> "yakkety"
        | `V17_04 -> "zesty"
        | `V17_10 -> "artful"
        | _ -> assert false in
      Some (D.tag_of_distro d, (apt_opam2 ?labels ~distro:"ubuntu" ~tag ()))
  | _ -> None
end

module Phases = struct

open Rresult
open R.Infix

let phase1 arch build_dir logs_dir () =
  ignore(C.Docker.exists ()); (* TODO rresult *)
  let build_dir = Fpath.v build_dir in
  let logs_dir = Fpath.v logs_dir in
  let joblog = Fpath.(logs_dir / "joblog.txt") in
  Bos.OS.Dir.create ~path:true build_dir >>= fun _ ->
  Bos.OS.Dir.create ~path:true logs_dir >>= fun _ ->
  let d =
    List.filter (D.distro_supported_on arch) D.active_distros |>
    List.map Gen.gen_opam_for_distro |>
    List.fold_left (fun a -> function Some x -> x::a | None -> a) [] in
  D.generate_dockerfiles ~crunch:false (Fpath.to_string build_dir) d; (* TODO fpath build_dir *)
  let dockerfile = Fpath.(build_dir / "Dockerfile.{}") in
  let arch_s = match arch with `X86_64 -> "x86_64" | `Aarch64 -> "aarch64" in
  let tag = Fmt.strf "%s-opam-{}" arch_s in
  let cmd = C.Docker.build ~cache:false ~dockerfile ~tag (Fpath.v ".") in
  let args = List.map fst d |> Bos.Cmd.of_list in
  let t = C.Parallel.run ~retries:1 ~results:logs_dir ~joblog cmd args in
  Logs.debug (fun l -> l "cmd: %s" (Bos.Cmd.to_string t));
  C.run_out t >>= fun _ ->
  C.Parallel.Joblog.v joblog |> fun joblog ->
  Logs.debug (fun l -> l "joblog: %s" (Sexplib.Sexp.to_string_hum (C.Parallel.Joblog.sexp_of_t joblog)));
  R.ok ()

let _ocaml_versions = D.stable_ocaml_versions
end

open Cmdliner
let setup_logs = C.setup_logs ()

let phase1_cmd =
  let logs_dir =
    let doc = "Directory in which to store logs" in
    Arg.(value & opt file "_logs" & info ["l";"logs-dir"] ~docv:"LOG_DIR" ~doc)
  in
  let arch =
    let doc = "CPU architecture to perform build on" in
    let term = Arg.enum ["x86_64",`X86_64; "aarch64",`Aarch64] in
    Arg.(value & opt term `X86_64 & info ["arch"] ~docv:"ARCH" ~doc)
  in
  let build_dir = 
    let doc = "Directory in which to store build artefacts" in
    Arg.(value & opt file "_build" & info ["b";"build-dir"] ~docv:"BUILD_DIR" ~doc)
  in
  let doc = "generate and build base opam container images" in
  let exits = Term.default_exits in
  let man = [
    `S Manpage.s_description;
    `P "Generate and build base $(b,opam) container images." ]
  in
  Term.(term_result (const Phases.phase1 $ arch $ build_dir $ logs_dir $ setup_logs)),
  Term.info "phase1" ~doc ~sdocs:Manpage.s_common_options ~exits ~man

let default_cmd =
  let doc = "build and push opam and OCaml multiarch container images" in
  let sdocs = Manpage.s_common_options in
  Term.(ret (const (fun _ -> `Help (`Pager, None)) $ pure ())),
  Term.info "obi-docker" ~version:"v1.0.0" ~doc ~sdocs

let cmds = [phase1_cmd]
let () = Term.(exit @@ eval_choice default_cmd cmds)
 
