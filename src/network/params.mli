type t = { 
	genesis		: string;
	magic		: string;
	port		: int;
	seeds		: string list;
}

type e = BTC | XTN | SIDECHAIN

val params_of_network 	: e -> t
val name_of_network 	: e -> string

