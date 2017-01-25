open Message
open Params

type status = 
	  CONNECTED
	| DISCONNECTED
	| WAITPING of int64
	

type t = {
	socket				: Unix.file_descr;
	address 			: Unix.inet_addr;
	port				: int;
	params				: Params.t;

	mutable received	: int;
	mutable sent		: int;
	
	mutable status		: status;
	mutable last_seen	: float;
	mutable height		: int32;
	mutable user_agent	: string;
}

val create		: Params.t -> Unix.inet_addr -> int -> t
val connect 	: t -> status
val send		: t -> Message.t -> unit
val recv		: t -> Message.t option
val handshake	: t -> unit
val disconnect	: t -> unit
val start		: t -> Blockchain.t -> unit