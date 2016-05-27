open Stdint;;
open Bitstring;;
open Crypto;;

module Header = struct
	type t = {
		hash		: Hash.t;
		version		: int32;
		prev_block	: Hash.t;
		merkle_root : Hash.t;
		timestamp	: float;
		bits		: int32;
		nonce		: int32;	
	};;
	
	let parse data = 
		let bdata = bitstring_of_string data in
		bitmatch bdata with 
		| {
			version 	: 4*8 : littleendian;
			prev_block	: 32*8: string; 
			merkle_root	: 32*8: string;
			timestamp	: 4*8 : string;
			bits		: 4*8 : littleendian;
			nonce		: 4*8 : littleendian
		} ->
		{
			hash			= Hash.of_bin (hash256 data);
			version			= version;
			prev_block		= Hash.of_binblock prev_block;
			merkle_root		= Hash.of_bin merkle_root;
			timestamp		= Uint32.to_float (Uint32.of_bytes_little_endian timestamp 0);
			bits			= bits;
			nonce			= nonce
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