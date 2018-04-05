open Bitcoinml;;
open Stdint;;

module Chainstate = struct
	type t = {
		mutable block           : Hash.t;
		mutable height        	: uint32;
		mutable prune_height    : uint32;

		mutable header        	: Hash.t;
		mutable header_height 	: uint32;

		mutable txs				: uint64;
		mutable utxos			: uint64;

		mutable difficulty		: uint64;
		mutable reward			: uint64;

		mutable address_index : bool;
	};;

	let serialize cs = 
		Bitstring.string_of_bitstring [%bitstring {|
			Hash.to_bin cs.block					: 32*8 : string;
			Uint32.to_int32 cs.height			: 32 : littleendian;
			Uint32.to_int32 cs.prune_height		: 32 : littleendian;
			Hash.to_bin cs.header             : 32*8 : string;
			Uint32.to_int32 cs.header_height  : 32 : littleendian;
			Uint64.to_int64 cs.txs			  		: 64 : littleendian;
			Uint64.to_int64 cs.utxos		  		: 64 : littleendian;
			Uint64.to_int64 cs.difficulty	  	: 64 : littleendian;
			Uint64.to_int64 cs.reward	  			: 64 : littleendian;
			cs.address_index : 1
		|}]
	;;

	let parse csb = 
		let bdata = Bitstring.bitstring_of_string csb in
		match%bitstring bdata with
		| {|
			block 		    	: 32*8 : string;
			height          : 32 : string;
			prune_height    : 32 : string;
			header 	        : 32*8 : string;
			header_height		: 32 : string;
			txs							: 64 : string;
			utxos						: 64 : string;
			difficulty			: 64 : string;
			reward					: 64 : string;
			address_index		: 1
		|} ->
		Some ({
			block 		    = Hash.of_bin block;
			height 	    	= Uint32.of_bytes_little_endian height 0;
			prune_height 	= Uint32.of_bytes_little_endian prune_height 0;
			header	    	= Hash.of_bin header;
			header_height	= Uint32.of_bytes_little_endian header_height 0;
			txs						= Uint64.of_bytes_little_endian txs 0;
			utxos					= Uint64.of_bytes_little_endian utxos 0;
			difficulty		= Uint64.of_bytes_little_endian difficulty 0;
			reward				= Uint64.of_bytes_little_endian reward 0;
			address_index = address_index;
		})
	;;
end


module Storage_index = Store.Make_index 
  (Chainstate) 
  (struct let prefix = "chainstate" end)
;;
