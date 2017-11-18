(* Copyright (c) 2017 Anil Madhavapeddy
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

(** Manipulate, parse and generate OCaml version strings *)

type t
(** Type of an OCaml version string *)

val major : t -> int
(** [major t] will return the major version number of an OCaml
    release.  For example, [of_string "4.03.0" |> major] will
    return [4]. *)

val minor : t -> int
(** [minor t] will return the minor version number of an OCaml
    release.  For example, [of_string "4.03.0" |> minor] will
    return [3]. *)

val patch : t -> int option
(** [patch t] will return the patch version number of an OCaml
    release.  For example, [of_string "4.03.0" |> minor] will
    return [Some 0]. *)

val extra : t -> string option
(** [extra t] will return the additional information string of
    an OCaml release.
    For example, [of_string "4.03.0+flambda" |> extra] will
    return [Some "flambda"]. *)


val v : ?patch:int -> ?extra:string -> int -> int -> t


val to_string : ?sep:char -> t -> string
val of_string : string -> t
val compare : t -> t -> int
val sys_version : t
val pp : Format.formatter -> t -> unit

val with_variant : t -> string option -> t

type arch = [ `X86_64 | `Aarch64 ]
val arches : arch list
val string_of_arch : arch -> string
val arch_of_string : string -> (arch, [> `Msg of string ]) result

module Releases : sig
  val v4_00_1 : t
  val v4_01_0 : t
  val v4_02_0 : t
  val v4_02_1 : t
  val v4_02_2 : t
  val v4_02_3 : t
  val v4_03_0 : t
  val v4_04_0 : t
  val v4_04_1 : t
  val v4_04_2 : t
  val v4_05_0 : t

  val all : t list
  val dev : t list
  val all_major : t list
  val recent_major : t list
  val recent_major_and_dev : t list
  val latest_major: t
end

module Since : sig
  val bytes: t
  val arch : arch -> t 
end

module Has : sig
  val bytes : t -> bool
  val arch : arch -> t -> bool
end

module Opam : sig
  val variants : t -> string list
  val default_variant : t -> string option

  val switches : t -> t list
  val default_switch : t -> t
  val variant_switches : t -> t list
end
