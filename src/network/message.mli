open Blockchain
open Stdint
open Bitcoinml


type header  = {
	magic		: int32;
	command		: string;
	length		: uint32;
	checksum	: string;
}

type invvect = 
	INV_ERROR
	| INV_TX of Hash.t
	| INV_BLOCK of Hash.t
	| INV_FILTERED_BLOCK of Hash.t

type addr = {
	services	: uint64;
	address		: string;
	port		: uint16;
}

type inv = invvect list

type getdata = inv
type notfound = inv

type version = {
	version		: int32;
	services	: int64;
	time		: float;
	addr_recv	: addr;
	addr_from	: addr;
	nonce		: int64;
	user_agent	: string;
	start_height: int32;
	relay		: bool;
}



type getheaders = {
	version		: int32;
	hashes		: Hash.t list;
	stop		: Hash.t;
}

type getblocks = getheaders


type headers = Block.Header.t list

type ping = int64
type pong = int64


type t = 
	| INVALID
	| VERSION of version
	| VERACK
	| PING of ping
	| PONG of pong
	| INV of inv
	| ADDR
	| GETDATA of inv
	| NOTFOUND of notfound
	| GETBLOCKS of getblocks
	| GETHEADERS of getheaders
	| TX of Tx.t
	| BLOCK of Block.t
	| HEADERS of headers
	| GETADDR
	| MEMPOOL
	| REJECT
	| FEEFILTER of Uint64.t
	| SENDHEADERS
	
	(* Bloom filter related *)
	| FILTERLOAD
	| FILTERADD
	| FILTERCLEAR
	| MERKLEBLOCK
	
	| ALERT
;;


val string_of_command : t -> string


val parse		: header -> bytes -> t
val parse_header: bytes -> header

val bitstring_of_addr 	: addr -> Bitstring.t
val serialize			: Params.t -> t -> bytes
