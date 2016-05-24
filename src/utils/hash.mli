type t = bytes
type hash = t

val to_string 		: bytes -> string
val bhash_to_string : bytes -> string

val from_string	: string -> bytes
val reverse		: string -> string