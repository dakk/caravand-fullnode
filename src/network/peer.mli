open Message
open Blockchain
open Utils
open Bitcoinml
open Stdint

type status = 
	| CONNECTED
	| DISCONNECTED
	| WAITPING of int64
	

type t = {
	socket		: Unix.file_descr;
	address 	: Unix.inet_addr;
	port		: int;
	params		: Params.t;
	config		: Config.t;

	mutable received	: int;
	mutable sent		: int;
	
	mutable status		: status;
	mutable last_seen	: float;
	mutable height		: int32;
	mutable user_agent: string;
	mutable fee_rate	:	Uint64.t;
	mutable send_headers : bool;
}

val create		: Params.t -> Config.t -> Unix.inet_addr -> int -> t
val connect 	: t -> status
val send		: t -> Message.t -> unit
val recv		: t -> Message.t option
val handshake	: t -> Int64.t -> unit
val disconnect	: t -> unit
val start		: t -> Chain.t -> unit
val handle		: t -> Chain.t -> unit