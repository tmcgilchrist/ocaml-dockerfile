(* generate ocaml docker containers *)
module L = Dockerfile_linux
module D = Dockerfile_distro
module C = Dockerfile_cmd
module G = Dockerfile_gen

let arch_to_docker = function
 | `X86_64 -> "amd64"
 | `Aarch64 -> "arm64"

module Log_gen = struct

  let phases = [ "phase1-arm64"; "phase1-amd64"; "phase2" ]
  
  let render_joblog f =
    let open C.Parallel.Joblog in
    v f |>
    List.map (fun j ->
    let result =
      if j.exit_code = 0 then "ok" else "fail"
    in
    Fmt.strf "
      <div class=\"joblog\">
        <div class=\"joblog_result\">%s</div>
        <div class=\"joblog_arg\">%s %s</div>
        <div class=\"joblog_runtime\">(%.02fs)</div>
      </div>
    " result j.command j.arg j.run_time
    )
end

module Gen = struct
  open Dockerfile
  open Dockerfile_opam
  (* Apk based Dockerfile *)
  let apk_opam2 ?(labels=[]) ~distro ~tag () =
    header distro tag @@
    label (("distro_style", "apk")::labels) @@
    L.Apk.install "build-base bzip2 git tar curl ca-certificates" @@
    install_opam_from_source ~install_wrappers:true ~branch:"master" () @@
    run "strip /usr/local/bin/opam*" @@
    from ~tag distro @@
    copy ~from:"0" ~src:["/usr/local/bin/opam"] ~dst:"/usr/bin/opam" () @@
    copy ~from:"0" ~src:["/usr/local/bin/opam-installer"] ~dst:"/usr/bin/opam-installer" () @@
    L.Apk.install "build-base tar ca-certificates git rsync curl sudo" @@ 
    L.Apk.add_user ~sudo:true "opam" @@
    L.Git.init () @@
    run "git clone git://github.com/ocaml/opam-repository /home/opam/opam-repository"

  (* Debian based Dockerfile *)
  let apt_opam2 ?(labels=[]) ~distro ~tag () =
    header distro tag @@
    label (("distro_style", "apt")::labels) @@
    L.Apt.install "build-essential curl git" @@
    install_opam_from_source ~install_wrappers:true ~branch:"master" () @@
    from ~tag distro @@
    copy ~from:"0" ~src:["/usr/local/bin/opam"] ~dst:"/usr/bin/opam" () @@
    copy ~from:"0" ~src:["/usr/local/bin/opam-installer"] ~dst:"/usr/bin/opam-installer" () @@
    L.Apt.install "build-essential curl git rsync sudo unzip" @@
    L.Git.init () @@
    run "git clone git://github.com/ocaml/opam-repository /home/opam/opam-repository"

  (* RPM based Dockerfile *)
  let yum_opam2 ?(labels=[]) ~distro ~tag () =
    header distro tag @@
    label (("distro_style", "apt")::labels) @@
    L.RPM.update @@
    L.RPM.dev_packages ~extra:"which tar curl xz" () @@
    install_opam_from_source ~prefix:"/usr" ~install_wrappers:true ~branch:"master" () @@
    from ~tag distro @@
    L.RPM.update @@
    L.RPM.dev_packages ~extra:"which tar curl xz" () @@
    copy ~from:"0" ~src:["/usr/bin/opam"] ~dst:"/usr/bin/opam" () @@
    copy ~from:"0" ~src:["/usr/bin/opam-installer"] ~dst:"/usr/bin/opam-installer" () @@
    L.RPM.add_user ~sudo:true "opam" @@ (** TODO pin uid at 1000 *)
    L.Git.init () @@
    run "git clone git://github.com/ocaml/opam-repository /home/opam/opam-repository"

  (* Zypper based Dockerfile *)
  let zypper_opam2 ?(labels=[]) ~distro ~tag () =
    header distro tag @@
    label (("distro_style", "zypper")::labels) @@
    L.Zypper.dev_packages () @@
    install_opam_from_source ~prefix:"/usr" ~install_wrappers:true ~branch:"master" () @@
    from ~tag distro @@
    L.Zypper.dev_packages () @@
    copy ~from:"0" ~src:["/usr/bin/opam"] ~dst:"/usr/bin/opam" () @@
    copy ~from:"0" ~src:["/usr/bin/opam-installer"] ~dst:"/usr/bin/opam-installer" () @@
    L.Zypper.add_user ~sudo:true "opam" @@
    L.Git.init () @@
    run "git clone git://github.com/ocaml/opam-repository /home/opam/opam-repository"

  (* Generate archive mirror *)
  let opam2_mirror (hub_id:string) =
    header hub_id "alpine-3.6-opam" @@
    run "sudo apk add --update bash m4" @@
    workdir "/home/opam/opam-repository" @@
    run "git pull origin master" @@
    run "opam admin upgrade" @@
    run "opam init -a /home/opam/opam-repository" @@
    run "opam install -yj4 cohttp-lwt-unix" @@
    run "opam admin cache"

  let all_ocaml_compilers hub_id arch distro =
    let distro = D.tag_of_distro distro in
    let compilers =
      D.stable_ocaml_versions |>
      List.filter (D.ocaml_supported_on arch) |>
      List.map D.ocaml_version_to_opam_switch |>
      List.map (run "opam switch create %s") |> (@@@) empty in
    let d = 
      header hub_id (Fmt.strf "%s-opam" distro) @@
      run "cd /home/opam/opam-repository && git pull origin master" @@
      run "opam init -a /home/opam/opam-repository" @@
      compilers @@
      run "opam switch default" in
    (Fmt.strf "%s-ocaml" distro), d

  let separate_ocaml_compilers hub_id arch distro =
    let distro = D.tag_of_distro distro in
    D.stable_ocaml_versions |>
    List.filter (D.ocaml_supported_on arch) |>
    List.map (fun ov ->
      let d = 
        header hub_id (Fmt.strf "%s-opam" distro) @@
        run "cd /home/opam/opam-repository && git pull origin master" @@
        run "opam init -a /home/opam/opam-repository -c %s" (D.ocaml_version_to_opam_switch ov) in
      (Fmt.strf "%s-ocaml-%s" distro (D.tag_of_ocaml_version ov)), d
    )

  let gen_opam_for_distro ?labels d =
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

   let multiarch_manifest ~target ~platforms =
     let ms =
       List.map (fun (image, arch) ->
         Fmt.strf "  -\n    image: %s\n    platform:\n      architecture: %s\n      os: linux" image arch
       ) platforms |> String.concat "\n" in
     Fmt.strf "image: %s\nmanifests:\n%s" target ms
end

module Phases = struct

  open Rresult
  open R.Infix

  let setup_log_dirs ~prefix build_dir logs_dir fn =
    Fpath.(build_dir / prefix) |> fun build_dir ->
    Fpath.(logs_dir / prefix) |> fun logs_dir ->
    Bos.OS.Dir.create ~path:true build_dir >>= fun _ ->
    Bos.OS.Dir.create ~path:true logs_dir >>= fun _ ->
    let md = C.Mdlog.init ~logs_dir ~prefix ~descr:prefix in (* TODO descr *)
    fn build_dir md >>= fun () ->
    C.Mdlog.output md

  (* Generate base opam binaries for all distros *)
  let phase1 cache push arch hub_id build_dir logs_dir () =
    let arch_s = arch_to_docker arch in
    let prefix = Fmt.strf "phase1-%s" arch_s in
    setup_log_dirs ~prefix build_dir logs_dir @@ fun build_dir md ->
    let tag = Fmt.strf "%s:{}-opam-linux-%s" hub_id arch_s in
    List.filter (D.distro_supported_on arch) D.active_distros |>
    List.map Gen.gen_opam_for_distro |> fun ds ->
    G.generate_dockerfiles ~crunch:true build_dir ds >>= fun () ->
    let dockerfile = Fpath.(build_dir / "Dockerfile.{}") in
    let cmd = C.Docker.build_cmd ~cache ~dockerfile ~tag (Fpath.v ".") in
    let args = List.map fst ds in
    C.Mdlog.run_parallel ~retries:1 md "01-build" cmd args >>= fun jobs ->
    if push then begin
      let cmd = C.Docker.push_cmd tag in
      C.Mdlog.run_parallel ~retries:1 md "02-push" cmd args
    end else Ok ()

  (* Push multiarch images to the Hub for base opam binaries *)
  let phase2 hub_id build_dir logs_dir () =
    setup_log_dirs ~prefix:"phase2" build_dir logs_dir @@ fun build_dir md ->
    let yaml_file tag = Fpath.(build_dir / (tag ^ ".yml")) in
    let yamls =
      List.map (fun distro ->
        let tag = D.tag_of_distro distro in
        let target = Fmt.strf "%s:%s-opam" hub_id tag in
        let platforms =
          D.distro_arches distro |>
          List.map (fun arch ->
            let arch = arch_to_docker arch in
            let image = Fmt.strf "%s:%s-opam-linux-%s" hub_id tag arch in
            image, arch) in
        Gen.multiarch_manifest ~target ~platforms |> fun m ->
        tag, m
      ) D.active_distros in
    C.iter (fun (t,m) -> Bos.OS.File.write (yaml_file t) m) yamls >>= fun () ->
    let cmd = C.Docker.manifest_push_file (yaml_file "{}") in
    let args = List.map (fun (t,_) -> t) yamls in
    C.Mdlog.run_parallel ~retries:1 md "01-manifest" cmd args >>= fun _ ->
    Ok ()

  (* Generate an opam archive suitable for pointing local builds at *)
  let phase3_archive cache push hub_id build_dir logs_dir () =
    setup_log_dirs ~prefix:"phase3-archive" build_dir logs_dir @@ fun build_dir md ->
    G.generate_dockerfile ~crunch:true build_dir (Gen.opam2_mirror hub_id) >>= fun () ->
    let dockerfile = Fpath.(build_dir / "Dockerfile") in
    let cmd = C.Docker.build_cmd ~cache ~dockerfile ~tag:"{}" (Fpath.v ".") in
    let args = ["opam2-archive"] in
    C.Mdlog.run_parallel ~retries:1 md "01-build" cmd args >>= fun () ->
    if push then begin
      let cmd = C.Docker.push_cmd "{}" in
      C.Mdlog.run_parallel ~retries:1 md "02-push" cmd args
    end else Ok ()

  (* Generate a single container with all the ocaml compilers present *)
  let phase3_megaocaml cache push arch hub_id build_dir logs_dir () =
    let arch_s = arch_to_docker arch in
    let prefix = Fmt.strf "phase3-megaocaml-%s" arch_s in
    setup_log_dirs ~prefix build_dir logs_dir @@ fun build_dir md ->
    let d =
      List.filter (D.distro_supported_on arch) D.active_distros |>
      List.map (Gen.all_ocaml_compilers hub_id arch) in
    G.generate_dockerfiles ~crunch:true build_dir d >>= fun () ->
    let dockerfile = Fpath.(build_dir / "Dockerfile.{}") in
    let tag = Fmt.strf "%s:{}-linux-%s" hub_id arch_s in
    let cmd = C.Docker.build_cmd ~cache ~dockerfile ~tag (Fpath.v ".") in
    let args = List.map fst d in
    C.Mdlog.run_parallel ~retries:1 md "01-build" cmd args >>= fun () ->
    if push then begin
      let cmd = C.Docker.push_cmd tag in
      C.Mdlog.run_parallel ~retries:1 md "02-push" cmd args
    end else Ok ()

  let phase3_ocaml cache push arch hub_id build_dir logs_dir () =
    let arch_s = arch_to_docker arch in
    let prefix = Fmt.strf "phase3-ocaml-%s" arch_s in
    setup_log_dirs ~prefix build_dir logs_dir @@ fun build_dir md ->
    let d =
      List.filter (D.distro_supported_on arch) D.active_distros |>
      List.map (Gen.separate_ocaml_compilers hub_id arch) |>
      List.flatten in
    G.generate_dockerfiles ~crunch:true build_dir d >>= fun () ->
    let dockerfile = Fpath.(build_dir / "Dockerfile.{}") in
    let tag = Fmt.strf "%s:{}-linux-%s" hub_id arch_s in
    let cmd = C.Docker.build_cmd ~cache ~dockerfile ~tag (Fpath.v ".") in
    let args = List.map fst d in
    C.Mdlog.run_parallel ~delay:5.0 ~retries:1 md "01-build" cmd args >>= fun () ->
    if push then begin
      let cmd = C.Docker.push_cmd tag in
      C.Mdlog.run_parallel ~retries:1 md "02-push" cmd args
    end else Ok ()

  (* Push multiarch images to the Hub for ocaml binaries *)
  let phase4 hub_id build_dir logs_dir () =
    setup_log_dirs ~prefix:"phase4" build_dir logs_dir @@ fun build_dir md ->
    let yaml_file tag = Fpath.(build_dir / (tag ^ ".yml")) in
    let yamls =
      List.map (fun distro ->
        let tag = D.tag_of_distro distro in
        let mega_ocaml =
          let target = Fmt.strf "%s:%s-ocaml" hub_id tag in
          let platforms =
            D.distro_arches distro |>
            List.map (fun arch ->
              let arch = arch_to_docker arch in
              let image = Fmt.strf "%s:%s-ocaml-linux-%s" hub_id tag arch in
              image, arch) in
          let tag = Fmt.strf "%s-ocaml" tag in
          Gen.multiarch_manifest ~target ~platforms |> fun m ->
          tag, m in
        let each_ocaml = List.map (fun ov ->
          let target = Fmt.strf "%s:%s-ocaml-%s" hub_id tag ov in
          let platforms =
            D.distro_arches distro |>
            List.filter (fun a -> Ocaml_version.(Has.arch a (of_string ov))) |>
            List.map (fun arch ->
              let arch = arch_to_docker arch in
              let image = Fmt.strf "%s:%s-ocaml-%s-linux-%s" hub_id tag ov arch in
              image, arch) in
          let tag = Fmt.strf "%s-ocaml-%s" tag ov in
          Gen.multiarch_manifest ~target ~platforms |> fun m ->
          tag,m 
        ) D.stable_ocaml_versions in
        mega_ocaml :: each_ocaml
      ) D.active_distros |> List.flatten in
    C.iter (fun (t,m) -> Bos.OS.File.write (yaml_file t) m) yamls >>= fun () ->
    let cmd = C.Docker.manifest_push_file (yaml_file "{}") in
    let args = List.map (fun (t,_) -> t) yamls in
    C.Mdlog.run_parallel ~retries:1 md "01-manifest" cmd args
end

open Cmdliner
let setup_logs = C.setup_logs ()

let hub_id =
  let doc = "Docker Hub user/repo to push to" in
  Arg.(value & opt string "ocaml/opam2-staging" & info ["hub-id"] ~docv:"HUB_ID" ~doc)

let fpath =
  Arg.conv ~docv:"PATH" (Fpath.of_string,Fpath.pp)

let cache =
  let doc = "Use Docker caching" in
  Arg.(value & opt bool false & info ["cache"] ~docv:"CACHE" ~doc)

let push =
  let doc = "Push result of builds to Docker Hub" in
  Arg.(value & opt bool true & info ["push"] ~docv:"PUSH" ~doc)

let build_dir = 
  let doc = "Directory in which to store build artefacts" in
  Arg.(value & opt fpath (Fpath.v "_build") & info ["b";"build-dir"] ~docv:"BUILD_DIR" ~doc)

let logs_dir =
  let doc = "Directory in which to store logs" in
  Arg.(value & opt fpath (Fpath.v "_logs") & info ["l";"logs-dir"] ~docv:"LOG_DIR" ~doc)

let arch =
  let doc = "CPU architecture to perform build on" in
  let term = Arg.enum ["x86_64",`X86_64; "aarch64",`Aarch64] in
  Arg.(value & opt term `X86_64 & info ["arch"] ~docv:"ARCH" ~doc)

let phase1_cmd =
  let doc = "generate, build and push base opam container images" in
  let exits = Term.default_exits in
  let man = [
    `S Manpage.s_description;
    `P "Generate and build base $(b,opam) container images." ]
  in
  Term.(term_result (const Phases.phase1 $ cache $ push $ arch $ hub_id $ build_dir $ logs_dir $ setup_logs)),
  Term.info "phase1" ~doc ~sdocs:Manpage.s_common_options ~exits ~man

let phase2_cmd =
  let doc = "combine opam container images into multiarch versions" in
  let exits = Term.default_exits in
  Term.(term_result (const Phases.phase2 $ hub_id $ build_dir $ logs_dir $ setup_logs)),
  Term.info "phase2" ~doc ~exits

let phase3_archive_cmd =
  let doc = "generate a distribution archive mirror" in
  let exits = Term.default_exits in
  Term.(term_result (const Phases.phase3_archive $ cache $ push $ hub_id $ build_dir $ logs_dir $ setup_logs)),
  Term.info "phase3-cache" ~doc ~exits

let phase3_megaocaml_cmd =
  let doc = "generate a ocaml compiler container with all the things" in
  let exits = Term.default_exits in
  Term.(term_result (const Phases.phase3_megaocaml $ cache $ push $ arch $ hub_id $ build_dir $ logs_dir $ setup_logs)),
  Term.info "phase3-megaocaml" ~doc ~exits

let phase3_ocaml_cmd =
  let doc = "generate a matrix of ocaml compilers" in
  let exits = Term.default_exits in
  Term.(term_result (const Phases.phase3_ocaml $ cache $ push $ arch $ hub_id $ build_dir $ logs_dir $ setup_logs)),
  Term.info "phase3-ocaml" ~doc ~exits

let phase4_cmd =
  let doc = "combine ocaml container images into multiarch versions" in
  let exits = Term.default_exits in
  Term.(term_result (const Phases.phase4 $ hub_id $ build_dir $ logs_dir $ setup_logs)),
  Term.info "phase4" ~doc ~exits

let default_cmd =
  let doc = "build and push opam and OCaml multiarch container images" in
  let sdocs = Manpage.s_common_options in
  Term.(ret (const (fun _ -> `Help (`Pager, None)) $ pure ())),
  Term.info "obi-docker" ~version:"v1.0.0" ~doc ~sdocs

let cmds = [phase1_cmd; phase2_cmd; phase3_archive_cmd; phase3_megaocaml_cmd; phase3_ocaml_cmd; phase4_cmd]
let () = Term.(exit @@ eval_choice default_cmd cmds)

