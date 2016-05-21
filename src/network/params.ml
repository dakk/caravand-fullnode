type e = BTC | XTN | SIDECHAIN;;

type t = { 
	genesis		: string;
	magic		: string;
	port		: int;
	seeds		: string list;
	network		: e;
};;


let of_network n =
	match n with
	| BTC -> 
		{ 
			network	= BTC;
			genesis	= "000000000019d6689c085ae165831e934ff763ae46a2a6c172b3f1b60a8ce26f";
			port	= 8333;
			magic	= "D9B4BEF9";
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

