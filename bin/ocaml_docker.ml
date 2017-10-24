(* generate ocaml docker containers *)
open Dockerfile
module L = Dockerfile_linux
module D = Dockerfile_distro
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
      
let _ = 
  (* TODO replace with cmdliner *)
  let arch =
    let s = try Sys.argv.(1) with _ -> "x86_64" in
    match s with
    "x86_64" -> `X86_64 | "aarch64" -> `Aarch64 |_ -> failwith "unknown arch"
  in
  let _ocaml_versions = D.stable_ocaml_versions in
  let d =
    List.filter (D.distro_supported_on arch) D.active_distros |>
    List.map gen_opam_for_distro |>
    List.fold_left (fun a -> function Some x -> x::a | None -> a) []
  in
  D.generate_dockerfiles ~crunch:false "output" d
