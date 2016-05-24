open Bitstring;;

module Header = struct
	type t = {
		hash		: Hash.t;
		version		: int32;
		prev_block	: Hash.t;
		merkle_root : Hash.t;
		timestamp	: float;
		bits		: int32;
		nonce		: int32;
		txn			: int64;	
	};;
	
	let parse data = 
		let bdata = bitstring_of_string data in
		bitmatch bdata with 
		| {
			version 	: 4*8 : littleendian;
			prev_block	: 32*8: string; 
			merkle_root	: 32*8: string;
			timestamp	: 4*8 : littleendian
		} ->
		{
			hash= data;
			version			= version;
			prev_block		= prev_block;
			merkle_root		= merkle_root;
			timestamp		= Int32.to_float timestamp;
			bits= Int32.of_int 12;
			nonce= Int32.of_int 12;
			txn= Int64.of_int 12;
		}
	;;
end

type t = {
	header	: Header.t;
	txs		: Tx.t list;
};;


let parse data = {
	header= Header.parse data;
	txs= [];
};;


let hash block = "";;

let serialize block = "";