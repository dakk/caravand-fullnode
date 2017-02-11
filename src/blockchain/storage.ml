open Stdint;;
open LevelDB;;
open Block;;
open Block.Header;;
open Tx;;

module Address = struct
	type t = {
		mutable balance			: uint64;
		mutable sent			: uint64;
		mutable received		: uint64;
		mutable txs				: uint64;
		(*txs					: Hash.t list;
		utxo					: (Hash.t * int32 * uint64) list*)
	}

	let parse data = 
		let bdata = Bitstring.bitstring_of_string data in
		bitmatch bdata with
		| { 
			balance			: 64 	: string;
			sent			: 64 	: string;
			received		: 64	: string;
			txs				: 64	: string
		} ->
		{
			balance		= Uint64.of_bytes_little_endian balance 0;
			sent		= Uint64.of_bytes_little_endian sent 0;
			received	= Uint64.of_bytes_little_endian received 0;
			txs			= Uint64.of_bytes_little_endian txs 0;
		}
	;;

	let serialize addr = 
		let bs = BITSTRING {
			Uint64.to_int64 addr.balance	: 64 : littleendian;
			Uint64.to_int64 addr.sent	  	: 64 : littleendian;
			Uint64.to_int64 addr.received	: 64 : littleendian;
			Uint64.to_int64 addr.txs	  	: 64 : littleendian
		} in Bitstring.string_of_bitstring bs
	;;

	let load_or_create db addr =
		let empty = {
			balance		= Uint64.zero;
			sent		= Uint64.zero;
			received	= Uint64.zero;
			txs			= Uint64.zero;
		} in
		let key = "adr_" ^ addr in
		if LevelDB.mem db key then 
			match LevelDB.get db key with
			| Some (d) -> parse d
			| None -> empty
		else 
			empty
	;;

	let save db addr data = 
		let key = "adr_" ^ addr in
		LevelDB.put db key @@ serialize data
	;;
end


module Chainstate = struct
	type t = {
		mutable block           : Hash.t;
		mutable height        	: uint32;

		mutable header        	: Hash.t;
		mutable header_height 	: uint32;

		mutable txs				: uint64;
		mutable utxos			: uint64;
	};;

	let serialize cs = 
		let bs = BITSTRING {
			Hash.to_bin cs.block				: 32*8 : string;
			Uint32.to_int32 cs.height			: 32 : littleendian;
			Hash.to_bin cs.header             	: 32*8 : string;
			Uint32.to_int32 cs.header_height  	: 32 : littleendian;
			Uint64.to_int64 cs.txs			  	: 64 : littleendian;
			Uint64.to_int64 cs.utxos		  	: 64 : littleendian
		} in Bitstring.string_of_bitstring bs
	;;

	let parse csb = 
		let bdata = Bitstring.bitstring_of_string csb in
		bitmatch bdata with
		| { 
			block 		    : 32*8 	: string;
			height          : 32 	: string;
			header 	        : 32*8 	: string;
			header_height	: 32 	: string;
			txs				: 64 	: string;
			utxos			: 64 	: string
		} ->
		{
			block 		    = Hash.of_bin block;
			height 	    	= Uint32.of_bytes_little_endian height 0;
			header	    	= Hash.of_bin header;
			header_height	= Uint32.of_bytes_little_endian header_height 0;
			txs				= Uint64.of_bytes_little_endian txs 0;
			utxos			= Uint64.of_bytes_little_endian utxos 0;
		}
	;;
end

open Chainstate;;

type t = {
	chainstate		:	Chainstate.t;
	db              :   LevelDB.db;
};;


let save_cs storage =
	let d = Chainstate.serialize storage.chainstate in
	LevelDB.put storage.db "chainstate" d 
;;

let sync storage =
	let d = Chainstate.serialize storage.chainstate in
	LevelDB.put ~sync:true storage.db "chainstate" d 
;;


let load path =
	let load_cs db =
		match LevelDB.get db "chainstate" with
		| Some (d) -> Chainstate.parse d
		| None -> {
			block= "0000000000000000000000000000000000000000000000000000000000000000";
			height= Uint32.of_int 0;
			header= "0000000000000000000000000000000000000000000000000000000000000000";
			header_height= Uint32.of_int 0;

			txs= Uint64.of_int 0;
			utxos= Uint64.of_int 0;
		}
	in
	let db = LevelDB.open_db path in 
	{
		chainstate= load_cs db;
		db= db
	}
;;

let close storage = 
	LevelDB.close storage.db
;;

let insert_header storage height (header : Block.Header.t) = 
	storage.chainstate.header_height <- Uint32.of_int64 height;

	LevelDB.put storage.db ("blk_" ^ header.hash) @@ Block.Header.serialize header;
	LevelDB.put storage.db ("bli_" ^ Printf.sprintf "%d" (Uint32.to_int storage.chainstate.header_height)) header.hash;
	storage.chainstate.header <- header.hash;

	save_cs storage    
;;

let insert_block storage height (block : Block.t) = 
	LevelDB.put storage.db ("blk_" ^ block.header.hash) @@ Block.serialize block;
	storage.chainstate.block <- block.header.hash;

	List.iteri (fun i tx -> 
		(* Insert tx *)
		let data = Bitstring.string_of_bitstring (BITSTRING {
			Hash.to_bin (block.header.hash)	: 32*8 : string;
			Int32.of_int i					: 32 : littleendian
		}) in
		LevelDB.put storage.db ("txi_" ^ tx.Tx.hash) data;
		storage.chainstate.txs <- Uint64.add storage.chainstate.txs Uint64.one;

		(* Insert utxo and user utxo, set balances *)
		List.iteri (fun i out -> 
			if Tx.Out.is_spendable out then (
				LevelDB.put storage.db ("utx_" ^ tx.Tx.hash ^ string_of_int i) @@ Tx.Out.serialize out;
				storage.chainstate.utxos <- Uint64.add storage.chainstate.utxos Uint64.one;

				(match Tx.Out.spendable_by out with
				| None -> ()
				| Some (addr) -> 
					let addrd = Address.load_or_create storage.db addr in
					addrd.txs <- Uint64.add addrd.txs @@ Uint64.one;
					addrd.received <- Uint64.add addrd.received @@ Uint64.of_int64 out.value;
					addrd.balance <- Uint64.add addrd.balance @@ Uint64.of_int64 out.value;
					Address.save storage.db addr addrd)
			)
		) tx.txout;


		(* Remove utxo and user utxo, set balances *)
		List.iter (fun ins -> 
			let key = "utx_" ^ ins.In.out_hash ^ string_of_int (Uint32.to_int ins.In.out_n) in
			if LevelDB.mem storage.db key then (
				match LevelDB.get storage.db key with
				| None -> ()
				| Some (utx) -> 
					let rest, utx = Tx.Out.parse @@ Bitstring.bitstring_of_string utx in
					match utx with
					| None -> ()
					| Some (utx) ->
						if Tx.Out.is_spendable utx then (
							(match Tx.Out.spendable_by utx with
							| None -> ()
							| Some (addr) -> 
								let addrd = Address.load_or_create storage.db addr in
								addrd.txs <- Uint64.add addrd.txs @@ Uint64.one;
								addrd.sent <- Uint64.add addrd.sent @@ Uint64.of_int64 utx.value;
								addrd.balance <- Uint64.sub addrd.balance @@ Uint64.of_int64 utx.value;
								Address.save storage.db addr addrd
							);

							LevelDB.delete storage.db key;
							storage.chainstate.utxos <- Uint64.sub storage.chainstate.utxos Uint64.one
						);
			)
		) tx.txin;
	) block.txs;
	
	storage.chainstate.height <- Uint32.of_int64 height;
	save_cs storage 
;;



let get_block storage hash = 
	match LevelDB.get storage.db ("blk_" ^ hash) with
	| Some (bdata) -> Block.parse bdata
	| None -> None
;;

let get_blocki storage height = 
	match LevelDB.get storage.db ("bli_" ^ Printf.sprintf "%d" (Int64.to_int height)) with
	| Some (h) -> get_block storage h 
	| None -> None
;;
	


let get_utx	storage tx index =
	match LevelDB.get storage.db ("utx_" ^ tx ^ string_of_int index) with
	| None -> None
	| Some (data) -> 
		match Tx.Out.parse (Bitstring.bitstring_of_string data) with
		| (rest, txo) -> txo
		| _ -> None
;;

let get_tx storage txhash =
	match LevelDB.get storage.db ("txi_" ^ txhash) with
	| None -> None
	| Some (data) -> 
		let bdata = Bitstring.bitstring_of_string data in
		bitmatch bdata with
		| { 
			blockhash  : 32*8 	: string;
			index      : 32 	: littleendian
		} ->
		let block = Hash.of_bin blockhash in
		match get_block storage block with
		| None -> None
		| Some (b) -> Some (List.nth b.txs @@ Int32.to_int index)
;;


let get_header storage hash = 
	match LevelDB.get storage.db ("blk_" ^ hash) with
	| None -> None
	| Some (data) -> Block.Header.parse @@ Bytes.sub data 0 80
;;

let get_headeri storage height = 
	match LevelDB.get storage.db ("bli_" ^ Printf.sprintf "%d" (Int64.to_int height)) with
	| Some (h) -> get_header storage h 
	| None -> None
;;

let get_blocks storage hashes = 
	let rec get_blocks' hs acc = match hashes with
	| [] -> acc
	| h::hs' -> match get_block storage h with
		| None -> get_blocks' hs' acc
		| Some (h') -> get_blocks' hs' (h'::acc)
	in get_blocks' hashes []
;;

let get_headers storage hashes = 
	let rec get_headers' hs acc = match hashes with
	| [] -> acc
	| h::hs' -> match get_header storage h with
		| None -> get_headers' hs' acc
		| Some (h') -> get_headers' hs' (h'::acc)
	in get_headers' hashes []
;;


let get_address storage addr = Address.load_or_create storage.db addr;;