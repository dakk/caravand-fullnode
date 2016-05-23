open Params;;

type header  = {
	magic		: int32;
	command		: string;
	length		: int32;
	checksum	: string;
};;


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

type ping = int64
type pong = int64


type t = 
	  VERSION of version
	| VERACK
	| PING of ping
	| PONG of pong
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


val parse		: header -> bytes -> t
val parse_header: bytes -> header
val serialize	: Params.t -> t -> bytes
