open Message
open Params

type t

val connect 	: Params.t -> Unix.inet_addr -> int -> t option
val send		: t -> Message.t -> unit
val recv		: t -> Message.t option
val handshake	: t -> unit