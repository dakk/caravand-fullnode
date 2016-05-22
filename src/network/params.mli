type e = BTC | XTN | SIDECHAIN

type t = { 
	services	: Int64.t;
	version		: int;
	genesis		: string;
	magic		: int;
	port		: int;
	seeds		: string list;
	network		: e;
}

val of_network 			: e -> t
val name_of_network 	: e -> string

