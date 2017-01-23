type e = BTC | XTN | SIDECHAIN


type genesis = {
	hash		: Hash.t;
	version		: int32;
	prev_block	: Hash.t;
	merkle_root : Hash.t;
	time	: float;
	bits		: int32;
	nonce		: int32;
}

type t = { 
	services		: Int64.t;
	version			: int;
	genesis			: genesis;
	magic			: int;
	port			: int;
	seeds			: string list;
	network			: e;
}

val of_network 			: e -> t
val name_of_network 	: e -> string

