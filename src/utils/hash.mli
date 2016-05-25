(* Hash in letchain are the readable string representation *)
type t = string
type hash = t
type b = bytes

val reverse		: string -> string

val to_bin			: t -> b
val to_binblock		: t -> b

val of_bin			: b -> t
val of_binblock 	: b -> t
val zero			: unit -> t