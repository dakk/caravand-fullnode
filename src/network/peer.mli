open Message
open Params

type t = {
	socket	: Unix.file_descr;
	address : Unix.inet_addr;
	port	: int;
	params	: Params.t;
}

val connect 	: Params.t -> Unix.inet_addr -> int -> t option
val send		: t -> Message.t -> unit
val recv		: t -> Message.t option
val handshake	: t -> unit