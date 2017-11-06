type t
val v : ?patch:int -> ?extra:string -> int -> int -> t
val to_string : ?sep:char -> t -> string
val of_string : string -> t
val compare : t -> t -> int
val sys_version : t
val pp : Format.formatter -> t -> unit

val with_variant : t -> string option -> t

type arch = [ `X86_64 | `Aarch64 ]
val arches : arch list

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
  val default_switch : t -> string
  val switches : t -> string list
end
