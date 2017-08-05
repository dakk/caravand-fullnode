open Stdint
open Bitcoinml

type e = BTC | XTN | BCH | SIDECHAIN | NOTFOUND


type genesis = {
	hash		: Hash.t;
	version		: int32;
	prev_block	: Hash.t;
	merkle_root : Hash.t;
	time		: float;
	bits		: uint32;
	nonce		: uint32;
}

type t = { 
	blocksize		: int;
	services		: Int64.t;
	version			: int;
	genesis			: genesis;
	magic				: int;
	port				: int;
	seeds				: string list;
	network			: e;
	checkpoints	: (int * Hash.t) list;
}

val of_network 			: e -> t
val name_of_network 	: e -> string
val abbr_to_network		: string -> e
