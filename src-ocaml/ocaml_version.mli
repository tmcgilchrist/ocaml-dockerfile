type t
val v : ?patch:int -> ?extra:string -> int -> int -> t
val to_string : t -> string
val of_string : string -> t
val compare : t -> t -> int
val t : t
val pp : Format.formatter -> t -> unit
type arch = [ `X86_64 | `Aarch64 ]
module Since : sig
  val bytes: t
  val arch : arch -> t 
end
module Has : sig
  val bytes : t -> bool
  val arch : arch -> t -> bool
end
