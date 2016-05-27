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
			genesis	= "000000000019d6689c085ae165831e934ff763ae46a2a6c172b3f1b60a8ce26f";
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
	| XTN -> 
		{ 
			version	= 70001;
			services= 0x0000000000000001L;
			network	= XTN;
			genesis	= "000000000933ea01ad0ee984209779baaec3ced90fa3f408719526f8d77f4943";
			port	= 18333;
			magic	= 0x0709110B;
			seeds	= [ 
				"testnet-seed.alexykot.me"; 
				"testnet-seed.bitcoin.petertodd.org";
				"testnet-seed.bluematt.me";
				"testnet-seed.bitcoin.schildbach.de"
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

