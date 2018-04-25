(*
 * Copyright (c) 2015 Anil Madhavapeddy <anil@recoil.org>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *
 *)

(** OPAM-specific Dockerfile rules *)

open Dockerfile
module Linux = Dockerfile_linux
module D = Dockerfile_distro
module OV = Ocaml_version

let run_as_opam fmt = Linux.run_as_user "opam" fmt

let install_opam_from_source ?(prefix= "/usr/local") ?(install_wrappers= false)
    ?(branch= "master") () =
  let wrappers_dir = Filename.concat prefix "share/opam" in
  let inst name =
    Printf.sprintf
      "cp shell/wrap-%s.sh %s && echo 'wrap-%s-commands: \"%s/wrap-%s.sh\"' >> /etc/opamrc.userns"
      name wrappers_dir name wrappers_dir name
  in
  let wrapper_cmd =
    match install_wrappers with
    | false -> "echo Not installing OPAM2 wrappers"
    | true ->
        Fmt.strf "mkdir -p %s && %s" wrappers_dir
          (String.concat " && " [inst "build"; inst "install"; inst "remove"])
  in
  run "git clone -b %s git://github.com/ocaml/opam /tmp/opam" branch
  @@ Linux.run_sh
       "cd /tmp/opam && make cold && mkdir -p %s/bin && cp /tmp/opam/opam %s/bin/opam && cp /tmp/opam/opam-installer %s/bin/opam-installer && chmod a+x %s/bin/opam %s/bin/opam-installer && %s && rm -rf /tmp/opam"
       prefix prefix prefix prefix prefix wrapper_cmd

let install_bubblewrap_from_source ?(prefix="/usr/local") () =
  let rel = "0.2.1" in
  let file = Fmt.strf "bubblewrap-%s.tar.xz" rel in
  let url = Fmt.strf "https://github.com/projectatomic/bubblewrap/releases/download/v%s/bubblewrap-%s.tar.xz" rel rel in
  run "curl -OL %s" url @@
  run "tar xf %s" file @@
  run "cd bubblewrap-%s && ./configure --prefix=%s && make && sudo make install" rel prefix @@
  run "rm -rf %s bubblewrap-%s" file rel

let disable_bubblewrap =
  run "sed -i -e 's/^wrap-/#wrap-/g' ~/.opam/config"

let enable_bubblewrap =
  run "sed -i -e 's/^#wrap-/wrap-/g' ~/.opam/config"

let header ?maintainer img tag =
  let maintainer =
    match maintainer with
    | None -> empty
    | Some t -> Dockerfile.maintainer "%s" t
  in
  comment "Autogenerated by OCaml-Dockerfile scripts" @@ from ~tag img
  @@ maintainer


(* Apk based Dockerfile *)
let apk_opam2 ?(labels= []) ~distro ~tag () =
  header distro tag @@ label (("distro_style", "apk") :: labels)
  @@ Linux.Apk.install "build-base bzip2 git tar curl ca-certificates"
  @@ install_opam_from_source ~install_wrappers:true ~branch:"master" ()
  @@ run "strip /usr/local/bin/opam*"
  @@ from ~tag distro
  @@ copy ~from:"0" ~src:["/usr/local/bin/opam"] ~dst:"/usr/bin/opam" ()
  @@ copy ~from:"0" ~src:["/usr/local/bin/opam-installer"]
       ~dst:"/usr/bin/opam-installer" ()
  @@ Linux.Apk.dev_packages ()
  @@ Linux.Apk.add_user ~uid:1000 ~sudo:true "opam" @@ Linux.Git.init ()
  @@ run
       "git clone git://github.com/ocaml/opam-repository /home/opam/opam-repository"


(* Debian based Dockerfile *)
let apt_opam2 ?(labels= []) ~distro ~tag () =
  header distro tag @@ label (("distro_style", "apt") :: labels)
  @@ Linux.Apt.install "build-essential curl git libcap-dev sudo"
  @@ install_bubblewrap_from_source ()
  @@ install_opam_from_source ~install_wrappers:true ~branch:"master" ()
  @@ from ~tag distro
  @@ copy ~from:"0" ~src:["/usr/local/bin/bwrap"] ~dst:"/usr/bin/bwrap" ()
  @@ copy ~from:"0" ~src:["/usr/local/bin/opam"] ~dst:"/usr/bin/opam" ()
  @@ copy ~from:"0" ~src:["/usr/local/bin/opam-installer"]
       ~dst:"/usr/bin/opam-installer" ()
  @@ Linux.Apt.dev_packages ()
  @@ Linux.Apt.add_user ~uid:1000 ~sudo:true "opam" @@ Linux.Git.init ()
  @@ run
       "git clone git://github.com/ocaml/opam-repository /home/opam/opam-repository"


(* RPM based Dockerfile *)
let yum_opam2 ?(labels= []) ~distro ~tag () =
  header distro tag @@ label (("distro_style", "apt") :: labels)
  @@ Linux.RPM.update 
  @@ Linux.RPM.dev_packages ~extra:"which tar curl xz libcap-devel" ()
  @@ install_bubblewrap_from_source ()
  @@ install_opam_from_source ~install_wrappers:true ~branch:"master" ()
  @@ install_opam_from_source ~prefix:"/usr" ~install_wrappers:true
       ~branch:"master" ()
  @@ from ~tag distro @@ Linux.RPM.update
  @@ Linux.RPM.dev_packages ()
  @@ copy ~from:"0" ~src:["/usr/local/bin/bwrap"] ~dst:"/usr/bin/bwrap" ()
  @@ copy ~from:"0" ~src:["/usr/bin/opam"] ~dst:"/usr/bin/opam" ()
  @@ copy ~from:"0" ~src:["/usr/bin/opam-installer"]
       ~dst:"/usr/bin/opam-installer" ()
  @@ run
       "sed -i.bak '/LC_TIME LC_ALL LANGUAGE/aDefaults    env_keep += \"OPAMYES OPAMJOBS OPAMVERBOSE\"' /etc/sudoers"
  @@ Linux.RPM.add_user ~uid:1000 ~sudo:true "opam" @@ Linux.Git.init ()
  @@ run
       "git clone git://github.com/ocaml/opam-repository /home/opam/opam-repository"


(* Zypper based Dockerfile *)
let zypper_opam2 ?(labels= []) ~distro ~tag () =
  header distro tag @@ label (("distro_style", "zypper") :: labels)
  @@ Linux.Zypper.dev_packages ()
  @@ install_bubblewrap_from_source ()
  @@ install_opam_from_source ~prefix:"/usr" ~install_wrappers:true
       ~branch:"master" ()
  @@ from ~tag distro
  @@ Linux.Zypper.dev_packages ()
  @@ copy ~from:"0" ~src:["/usr/local/bin/bwrap"] ~dst:"/usr/bin/bwrap" ()
  @@ copy ~from:"0" ~src:["/usr/bin/opam"] ~dst:"/usr/bin/opam" ()
  @@ copy ~from:"0" ~src:["/usr/bin/opam-installer"]
       ~dst:"/usr/bin/opam-installer" ()
  @@ Linux.Zypper.add_user ~uid:1000 ~sudo:true "opam" @@ Linux.Git.init ()
  @@ run
       "git clone git://github.com/ocaml/opam-repository /home/opam/opam-repository"


let gen_opam2_distro ?labels d =
  let fn =
    match D.resolve_alias d with
    | `Alpine v ->
        let tag =
          match v with
          | `V3_3 -> "3.3"
          | `V3_4 -> "3.4"
          | `V3_5 -> "3.5"
          | `V3_6 -> "3.6"
          | `V3_7 -> "3.7"
          | `Latest -> assert false
        in
        apk_opam2 ?labels ~distro:"alpine" ~tag ()
    | `Debian v ->
        let tag =
          match v with
          | `V7 -> "7"
          | `V8 -> "8"
          | `V9 -> "9"
          | `Testing -> "testing"
          | `Unstable -> "unstable"
          | `Stable -> assert false
        in
        apt_opam2 ?labels ~distro:"debian" ~tag ()
    | `Ubuntu v ->
        let tag =
          match v with
          | `V12_04 -> "precise"
          | `V14_04 -> "trusty"
          | `V15_04 -> "vivid"
          | `V15_10 -> "wily"
          | `V16_04 -> "xenial"
          | `V16_10 -> "yakkety"
          | `V17_04 -> "zesty"
          | `V17_10 -> "artful"
          | `V18_04 -> "bionic"
          | `Latest | `LTS -> assert false
        in
        apt_opam2 ?labels ~distro:"ubuntu" ~tag ()
    | `CentOS v ->
        let tag = match v with `V6 -> "6" | `V7 -> "7" | _ -> assert false in
        yum_opam2 ?labels ~distro:"centos" ~tag ()
    | `Fedora v ->
        let tag =
          match v with
          | `V21 -> "21"
          | `V22 -> "22"
          | `V23 -> "23"
          | `V24 -> "24"
          | `V25 -> "25"
          | `V26 -> "26"
          | `V27 -> "27"
          | `Latest -> assert false
        in
        yum_opam2 ?labels ~distro:"fedora" ~tag ()
    | `OracleLinux v ->
        let tag = match v with `V7 -> "7" | _ -> assert false in
        yum_opam2 ?labels ~distro:"oraclelinux" ~tag ()
    | `OpenSUSE v ->
        let tag =
          match v with
          | `V42_1 -> "42.1"
          | `V42_2 -> "42.2"
          | `V42_3 -> "42.3"
          | `Latest -> assert false
        in
        zypper_opam2 ?labels ~distro:"opensuse" ~tag ()
  in
  (D.tag_of_distro d, fn)


(* Generate archive mirror *)
let opam2_mirror (hub_id: string) =
  header hub_id "alpine-3.7-ocaml-4.06"
  @@ run "sudo apk add --update bash m4"
  @@ workdir "/home/opam/opam-repository" @@ run "git checkout master"
  @@ run "git pull origin master"
  @@ run "opam init -a /home/opam/opam-repository" @@ env [("OPAMJOBS", "24")]
  @@ run "opam install -yj4 cohttp-lwt-unix" @@ run "opam admin cache"

let opam_switches =
  run "curl -OL https://raw.githubusercontent.com/avsm/opam-switches/master/opam-switches" @@
  run "chmod a+x opam-switches" @@
  run "sudo mv opam-switches /usr/bin/opam-switches"

let all_ocaml_compilers hub_id arch distro =
  let distro_tag = D.tag_of_distro distro in
  let compilers =
    OV.Releases.recent |>
    List.filter (fun ov -> D.distro_supported_on arch ov distro) |>
    List.map OV.Opam.default_switch |>
    List.map (fun t -> Fmt.strf "%s:%s" (OV.(to_string (with_patch (with_variant t None) None))) (OV.Opam.V2.package t))
    |> fun ovs -> run "opam-switches create %s" (String.concat " " ovs)
  in
  let d =
    header hub_id (Fmt.strf "%s-opam" distro_tag)
    @@ workdir "/home/opam/opam-repository" @@ run "git pull origin master"
    @@ run "opam init -k git -a /home/opam/opam-repository --bare"
    @@ disable_bubblewrap
    @@ opam_switches
    @@ compilers
    @@ run "opam switch %s" (OV.(to_string (with_patch OV.Releases.latest None)))
    @@ entrypoint_exec ["opam"; "config"; "exec"; "--"]
    @@ cmd "bash"
  in
  (Fmt.strf "%s-ocaml" distro_tag, d)

let tag_of_ocaml_version ov =
  Ocaml_version.with_patch ov None |>
  Ocaml_version.to_string |>
  String.map (function '+' -> '-' | x -> x)

let separate_ocaml_compilers hub_id arch distro =
  let distro_tag = D.tag_of_distro distro in
  OV.Releases.recent_with_dev |> List.filter (fun ov -> D.distro_supported_on arch ov distro) 
  |> List.map (fun ov ->
         let default_switch = OV.Opam.default_switch ov in
         let default_switch_name = OV.(with_patch (with_variant default_switch None) None |> to_string) in
         let create_default_switch = run "opam switch create %s %s" default_switch_name OV.(to_string default_switch) in
         let variants =
           OV.Opam.variant_switches ov |>
           List.map (fun t -> Fmt.strf "%s:%s" (OV.(to_string (with_patch t None))) (OV.Opam.V2.package t)) |>
           fun ovs -> run "opam-switches create %s" (String.concat " " ovs)
         in
         let d =
           header hub_id (Fmt.strf "%s-opam" distro_tag)
           @@ workdir "/home/opam/opam-repository"
           @@ run "opam init -k git -a /home/opam/opam-repository --bare"
           @@ disable_bubblewrap
           @@ create_default_switch
           @@ opam_switches
           @@ variants
           @@ run "opam switch %s" default_switch_name
           @@ entrypoint_exec ["opam"; "config"; "exec"; "--"]
           @@ cmd "bash"
         in
         (Fmt.strf "%s-ocaml-%s" distro_tag (tag_of_ocaml_version ov), d) )


let bulk_build prod_hub_id distro ocaml_version opam_repo_rev =
  let ov_base = OV.(to_string (with_variant ocaml_version None)) in
  header prod_hub_id (Fmt.strf "%s-ocaml-%s" (D.tag_of_distro distro) ov_base)
  @@ run "opam switch %s" (OV.to_string ocaml_version)
  @@ env [("OPAMYES", "1")]
  @@ workdir "/home/opam/opam-repository"
  @@ run "git pull origin master"
  @@ run "git checkout %s" opam_repo_rev
  @@ run "opam update"
  @@ run "opam install -y depext"
  @@ run "opam depext -iy jbuilder ocamlfind"

let multiarch_manifest ~target ~platforms =
  let ms =
    List.map
      (fun (image, arch) ->
        Fmt.strf
          "  -\n    image: %s\n    platform:\n      architecture: %s\n      os: linux"
          image arch)
      platforms
    |> String.concat "\n"
  in
  Fmt.strf "image: %s\nmanifests:\n%s" target ms
