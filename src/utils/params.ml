type e = BTC | XTN | SIDECHAIN;;

type genesis = {
	hash		: Hash.t;
	version		: int32;
	prev_block	: Hash.t;
	merkle_root : Hash.t;
	time	: float;
	bits		: int32;
	nonce		: int32;
};;

type t = { 
	services		: Int64.t;
	version			: int;
	genesis			: genesis;
	magic			: int;
	port			: int;
	seeds			: string list;
	network			: e;
};;


let of_network n =
	match n with
	| BTC -> 
		{ 
			version	= 70001;
			services= 0x0000000000000001L;
			network	= BTC;

			genesis = { 
				hash		= "000000000019d6689c085ae165831e934ff763ae46a2a6c172b3f1b60a8ce26f";
				merkle_root	= "4a5e1e4baab89f3a32518a88c31bc87f618f76673e2cc77ab2127b7afdeda33b";
				prev_block 	= "0000000000000000000000000000000000000000000000000000000000000000";
				nonce		= Int32.of_int 2083236893;
				time		= 1231006505.0;
				bits		= Int32.of_int 0x1d00ffff;
				version 	= Int32.of_int 1;
			};

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

			genesis = { 
				hash		= "000000000933ea01ad0ee984209779baaec3ced90fa3f408719526f8d77f4943";
				merkle_root	= "4a5e1e4baab89f3a32518a88c31bc87f618f76673e2cc77ab2127b7afdeda33b";
				prev_block 	= "0000000000000000000000000000000000000000000000000000000000000000";
				nonce		= Int32.of_int 414098458;
				time		= 1296688602.0;
				bits		= Int32.of_int 0x1d00ffff;
				version 	= Int32.of_int 1;
			};

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

