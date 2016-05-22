open Params;;

type header

type version = {
	version		: int32;
	services	: int64;
	timestamp	: Unix.tm;
	addr_recv	: string;
	addr_from	: string;
	nonce		: int64;
	user_agent	: string;
	start_height: int32;
	relay		: bool;
}

(*type ping
type pong*)

type t = 
	  VERSION of version
	| VERACK
	| PING
	| PONG
	| INV
	| ADDR
	| GETDATA
	| NOTFOUND
	| GETBLOCKS
	| GETHEADERS
	| TX
	| BLOCKS
	| HEADERS
	| GETADDR
	| MEMPOOL
	| REJECT
	
	(* Bloom filter related *)
	| FILTERLOAD
	| FILTERADD
	| FILTERCLEAR
	| MERKLEBLOCK
	
	| ALERT
	| SENDHEADERS
;;



(* Parse the header from the given 24 bytes *)
val parse_header: bytes -> header

(* Parse the payload *)
val parse		: header -> bytes -> t

(* Serialize the message (the result include also the header) *)
val serialize	: Params.t -> t -> bytes