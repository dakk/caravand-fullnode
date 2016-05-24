type e = BTC | XTN | SIDECHAIN;;

type t = { 
	services	: Int64.t;
	version		: int;
	genesis		: string;
	magic		: int;
	port		: int;
	seeds		: string list;
	network		: e;
};;


let of_network n =
	match n with
	| BTC -> 
		{ 
			version	= 70001;
			services= 0x0000000000000001L;
			network	= BTC;
			genesis	= Hash.reverse "\x00\x00\x00\x00\x00\x19\xd6\x68\x9c\x08\x5a\xe1\x65\x83\x1e\x93\x4f\xf7\x63\xae\x46\xa2\xa6\xc1\x72\xb3\xf1\xb6\x0a\x8c\xe2\x6f";
			port	= 8333;
			magic	= 0xD9B4BEF9;
			seeds	= [ 
				(*"seed.bitcoin.sipa.be";*) 
				"dnsseed.bluematt.me"; 
				"dnsseed.bitcoin.dashjr.org"; 
				"seed.bitcoinstats.com"; 
				"bitseed.xf2.org"
			];
		}
	| _ -> failwith "Not available"
;;


let name_of_network n =
	match n with 
	| BTC -> "Bitcoin mainnet"
	| XTN -> "Bitcoin testnet"
	| _ -> ""
;;

