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
open Printf
module Linux = Dockerfile_linux
module D = Dockerfile_distro

let run_as_opam fmt = Linux.run_as_user "opam" fmt
let opamhome = "/home/opam"

let install_opam_from_source ?(prefix="/usr/local") ?(install_wrappers=false) ?(branch="1.2") () =
  run "git clone -b %s git://github.com/ocaml/opam /tmp/opam" branch @@
  let wrappers_dir = Filename.concat prefix "share/opam" in
  let inst name =
    Printf.sprintf "cp shell/wrap-%s.sh %s && echo 'wrap-%s-commands: \"%s/wrap-%s.sh\"' >> /etc/opamrc.userns" 
      name wrappers_dir name wrappers_dir name in
  let wrapper_cmd =
    match install_wrappers with
    | false -> "echo Not installing OPAM2 wrappers"
    | true -> Fmt.strf "mkdir -p %s && %s" wrappers_dir (String.concat " && " [inst "build"; inst "install"; inst "remove"])
  in
  Linux.run_sh
    "cd /tmp/opam && make cold && mkdir -p %s/bin && cp /tmp/opam/opam %s/bin/opam && cp /tmp/opam/opam-installer %s/bin/opam-installer && chmod a+x %s/bin/opam %s/bin/opam-installer && %s && rm -rf /tmp/opam"
    prefix prefix prefix prefix prefix
    wrapper_cmd

let header ?maintainer img tag =
  let maintainer = match maintainer with None -> empty | Some t -> Dockerfile.maintainer "%s" t in
  comment "Autogenerated by OCaml-Dockerfile scripts" @@
  from ~tag img @@
  maintainer

 (* Apk based Dockerfile *)
  let apk_opam2 ?(labels=[]) ~distro ~tag () =
    header distro tag @@
    label (("distro_style", "apk")::labels) @@
    Linux.Apk.install "build-base bzip2 git tar curl ca-certificates" @@
    install_opam_from_source ~install_wrappers:true ~branch:"master" () @@
    run "strip /usr/local/bin/opam*" @@
    from ~tag distro @@
    copy ~from:"0" ~src:["/usr/local/bin/opam"] ~dst:"/usr/bin/opam" () @@
    copy ~from:"0" ~src:["/usr/local/bin/opam-installer"] ~dst:"/usr/bin/opam-installer" () @@
    Linux.Apk.install "build-base tar ca-certificates git rsync curl sudo bash" @@ 
    Linux.Apk.add_user ~uid:1000 ~sudo:true "opam" @@
    Linux.Git.init () @@
    entrypoint_exec ["opam";"config";"exec";"--"] @@
    run "git clone git://github.com/ocaml/opam-repository /home/opam/opam-repository"

  (* Debian based Dockerfile *)
  let apt_opam2 ?(labels=[]) ~distro ~tag () =
    header distro tag @@
    label (("distro_style", "apt")::labels) @@
    Linux.Apt.install "build-essential curl git" @@
    install_opam_from_source ~install_wrappers:true ~branch:"master" () @@
    from ~tag distro @@
    copy ~from:"0" ~src:["/usr/local/bin/opam"] ~dst:"/usr/bin/opam" () @@
    copy ~from:"0" ~src:["/usr/local/bin/opam-installer"] ~dst:"/usr/bin/opam-installer" () @@
    Linux.Apt.install "build-essential curl git rsync sudo unzip" @@
    Linux.Apt.add_user ~uid:1000 ~sudo:true "opam" @@
    Linux.Git.init () @@
    entrypoint_exec ["opam";"config";"exec";"--"] @@
    run "git clone git://github.com/ocaml/opam-repository /home/opam/opam-repository"

  (* RPM based Dockerfile *)
  let yum_opam2 ?(labels=[]) ~distro ~tag () =
  let centos6_modern_git =
    match distro,tag with "TODOcentos","6" ->
    run "curl -OL http://packages.sw.be/rpmforge-release/rpmforge-release-0.5.2-2.el6.rf.x86_64.rpm" @@
    run "rpm --import http://apt.sw.be/RPM-GPG-KEY.dag.txt" @@
    run "rpm -K rpmforge-release-0.5.2-2.el6.rf.*.rpm" @@
    run "rpm -i rpmforge-release-0.5.2-2.el6.rf.*.rpm" @@
    run "rm -f rpmforge-release-0.5.2-2.el6.rf.*.rpm" @@
    run "yum -y --disablerepo=base,updates --enablerepo=rpmforge-extras update git"
    |_ -> empty in

    header distro tag @@
    label (("distro_style", "apt")::labels) @@
    Linux.RPM.update @@
    centos6_modern_git @@
    Linux.RPM.dev_packages ~extra:"which tar curl xz" () @@
    install_opam_from_source ~prefix:"/usr" ~install_wrappers:true ~branch:"master" () @@
    from ~tag distro @@
    Linux.RPM.update @@
    Linux.RPM.dev_packages ~extra:"which tar curl xz" () @@
    copy ~from:"0" ~src:["/usr/bin/opam"] ~dst:"/usr/bin/opam" () @@
    copy ~from:"0" ~src:["/usr/bin/opam-installer"] ~dst:"/usr/bin/opam-installer" () @@
    run "sed -i.bak '/LC_TIME LC_ALL LANGUAGE/aDefaults    env_keep += \"OPAMYES OPAMJOBS OPAMVERBOSE\"' /etc/sudoers" @@
    Linux.RPM.add_user ~uid:1000 ~sudo:true "opam" @@ (** TODO pin uid at 1000 *)
    Linux.Git.init () @@
    entrypoint_exec ["opam";"config";"exec";"--"] @@
    run "git clone git://github.com/ocaml/opam-repository /home/opam/opam-repository"

  (* Zypper based Dockerfile *)
  let zypper_opam2 ?(labels=[]) ~distro ~tag () =
    header distro tag @@
    label (("distro_style", "zypper")::labels) @@
    Linux.Zypper.dev_packages () @@
    install_opam_from_source ~prefix:"/usr" ~install_wrappers:true ~branch:"master" () @@
    from ~tag distro @@
    Linux.Zypper.dev_packages () @@
    copy ~from:"0" ~src:["/usr/bin/opam"] ~dst:"/usr/bin/opam" () @@
    copy ~from:"0" ~src:["/usr/bin/opam-installer"] ~dst:"/usr/bin/opam-installer" () @@
    Linux.Zypper.add_user ~uid:1000 ~sudo:true "opam" @@
    Linux.Git.init () @@
    entrypoint_exec ["opam";"config";"exec";"--"] @@
    run "git clone git://github.com/ocaml/opam-repository /home/opam/opam-repository"

  let gen_opam2_distro ?labels d =
    let fn =
     match D.resolve_alias d with
     | `Alpine v ->
      let tag = match v with
        | `V3_3 -> "3.3" | `V3_4 -> "3.4"
        | `V3_5 -> "3.5" | `V3_6 -> "3.6"
        | `Latest -> assert false in
      apk_opam2 ?labels ~distro:"alpine" ~tag ()
     | `Debian v ->
      let tag = match v with
        | `V7 -> "7"
        | `V8 -> "8"
        | `V9 -> "9"
        | `Testing -> "testing"
        | `Unstable -> "unstable"
        | `Stable -> assert false in
      apt_opam2 ?labels ~distro:"debian" ~tag ()
    | `Ubuntu v ->
      let tag = match v with
        | `V12_04 -> "precise"
        | `V14_04 -> "trusty"
        | `V16_04 -> "xenial"
        | `V16_10 -> "yakkety"
        | `V17_04 -> "zesty"
        | `V17_10 -> "artful"
        | _ -> assert false in
      apt_opam2 ?labels ~distro:"ubuntu" ~tag ()
   | `CentOS v ->
      let tag = match v with
        | `V6 -> "6"
        | `V7 -> "7"
        | _ -> assert false in
      yum_opam2 ?labels ~distro:"centos" ~tag ()
   | `Fedora v ->
      let tag = match v with
        | `V21 -> "21" | `V22 -> "22" | `V23 -> "23" | `V24 -> "24"
        | `V25 -> "25" | `V26 -> "26"
        | _ -> assert false in
      yum_opam2 ?labels ~distro:"fedora" ~tag ()
   | `OracleLinux v ->
      let tag = match v with
        | `V7 -> "7" 
        | _ -> assert false in
      yum_opam2 ?labels ~distro:"oraclelinux" ~tag ()
   | `OpenSUSE v ->
      let tag = match v with
        | `V42_1 -> "42.1"  | `V42_2 -> "42.2" | `V42_3 -> "42.3"
        | _ -> assert false in
      zypper_opam2 ?labels ~distro:"opensuse" ~tag ()
   in (D.tag_of_distro d), fn


