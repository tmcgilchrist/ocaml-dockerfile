(*
 * Copyright (c) 2015-2016 Anil Madhavapeddy <anil@recoil.org>
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

(** Rules for generating Dockerfiles involving OPAM *)

val run_as_opam : ('a, unit, string, Dockerfile.t) format4 -> 'a
(** [run_as_opam fmt] runs the command specified by the [fmt]
    format string as the [opam] user. *)

val install_opam_from_source :
  ?prefix:string -> ?install_wrappers:bool -> ?branch:string -> unit
  -> Dockerfile.t
(** Commands to install OPAM via a source code checkout from GitHub.
    The [branch] defaults to the [1.2] stable branch.
    The binaries are installed under [<prefix>/bin], defaulting to [/usr/local/bin].
    If [install_wrappers] is [true] then OPAM2 sandboxing scripts are installed (defaults to [false]). *)

val header : ?maintainer:string -> string -> string -> Dockerfile.t
(** [header image tag] initalises a fresh Dockerfile using the [image:tag]
    as its base. *)

val yum_opam2 :
  ?labels:(string * string) list -> distro:string -> tag:string -> unit
  -> Dockerfile.t

val zypper_opam2 :
  ?labels:(string * string) list -> distro:string -> tag:string -> unit
  -> Dockerfile.t

val apk_opam2 :
  ?labels:(string * string) list -> distro:string -> tag:string -> unit
  -> Dockerfile.t

val gen_opam2_distro :
  ?labels:(string * string) list -> Dockerfile_distro.t
  -> string * Dockerfile.t

val opam2_mirror : string -> Dockerfile.t

val all_ocaml_compilers :
  string -> Ocaml_version.arch -> Dockerfile_distro.t -> string * Dockerfile.t

val separate_ocaml_compilers :
  string -> Ocaml_version.arch -> Dockerfile_distro.t
  -> (string * Dockerfile.t) list

val bulk_build : string -> Dockerfile_distro.t -> Ocaml_version.t -> string -> (string * Dockerfile.t) list

val multiarch_manifest : target:string -> platforms:(string * string) list -> string
