open Params;;

type object_type =
	  MSG_ERROR 
	| MSG_TX
	| MSG_BLOCK
	| MSG_FILTERED_BLOCK


type header  = {
	magic		: int32;
	command		: string;
	length		: int32;
	checksum	: string;
}

type invvect = {
	itype		: object_type;
	hash		: bytes;
}

type addr = {
	services	: int64;
	address		: string;
	port		: int;
}

type inv = {
	count		: int64;
	inventory	: invvect list;
}

type version = {
	version		: int32;
	services	: int64;
	timestamp	: Unix.tm;
	addr_recv	: addr;
	addr_from	: addr;
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
	| INV of inv
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


val string_of_command : t -> string


val parse		: header -> bytes -> t
val parse_header: bytes -> header

val bitstring_of_addr 	: addr -> Bitstring.t
val serialize			: Params.t -> t -> bytes
