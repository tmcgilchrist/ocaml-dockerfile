type t
val v : ?patch:int -> ?extra:string -> int -> int -> t
val to_string : t -> string
val of_string : string -> t
val compare : t -> t -> int
val t : t
val pp : Format.formatter -> t -> unit
module Since : sig
  val bytes: t
  type arch = [ `X86_64 | `Aarch64 ]
  val arch_supported : arch -> t 
end
module Has : sig
  val bytes : t -> bool
end
