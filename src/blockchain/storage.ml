open Stdint;;
open LevelDB;;

module Chainstate = struct
	type t = {
		mutable block               : Hash.t;
		mutable height        : uint32;

		mutable header        : Hash.t;
		mutable header_height : uint32;
	};;

	let serialize cs = 
		let bs = BITSTRING {
			Hash.to_bin cs.block				: 32*8 : string;
			Uint32.to_int32 cs.height			: 32 : littleendian;
			Hash.to_bin cs.header             	: 32*8 : string;
			Uint32.to_int32 cs.header_height  	: 32 : littleendian
		} in Bitstring.string_of_bitstring bs
	;;

	let parse csb = 
		let bdata = Bitstring.bitstring_of_string csb in
		bitmatch bdata with
		| { 
			block 		    : 32*8 	: string;
			height          : 32 	: string;
			header 	        : 32*8 	: string;
			header_height	: 32 	: string
		} ->
		{
			block 		    = Hash.of_bin block;
			height 	    	= Uint32.of_bytes_little_endian height 0;
			header	    	= Hash.of_bin header;
			header_height	= Uint32.of_bytes_little_endian header_height 0;
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

let insert_header storage height hash header = 
	LevelDB.put storage.db ("blk_" ^ hash) header;
	LevelDB.put storage.db ("bli_" ^ Printf.sprintf "%d" (Int64.to_int height)) hash;
	storage.chainstate.header <- hash;
	storage.chainstate.header_height <- Uint32.of_int64 height;
	save_cs storage    
;;

let insert_block storage height hash block = 
	LevelDB.put storage.db ("blk_" ^ hash) block;
	storage.chainstate.block <- hash;
	storage.chainstate.height <- Uint32.of_int64 height;
	save_cs storage 
;;

let remove_last_block storage hash = 
	LevelDB.delete storage.db ("blk_" ^ hash);
	storage.chainstate.height <- Uint32.sub (storage.chainstate.height) Uint32.one;
	match LevelDB.get storage.db ("bli_" ^ Printf.sprintf "%d" (Uint32.to_int storage.chainstate.height)) with
	| None -> ()
	| Some (h) -> 
		storage.chainstate.block <- h;
		save_cs storage 
;;

let remove_last_header storage hash = 
	LevelDB.delete storage.db ("blk_" ^ hash);
	storage.chainstate.header_height <- Uint32.sub (storage.chainstate.header_height) Uint32.one;
	match LevelDB.get storage.db ("bli_" ^ Printf.sprintf "%d" (Uint32.to_int storage.chainstate.header_height)) with
	| None -> ()
	| Some (h) -> 
		storage.chainstate.header <- h;
		save_cs storage 
;;




let get_block storage hash = 
	LevelDB.get storage.db ("blk_" ^ hash)
;;

let get_blocki storage height = 
	match LevelDB.get storage.db ("bli_" ^ Printf.sprintf "%d" (Int64.to_int height)) with
	| Some (h) -> get_block storage h 
	| None -> None
;;
	


let insert_txi storage txhash blockhash offset len =
	let data = Bitstring.string_of_bitstring (BITSTRING {
		Hash.to_bin blockhash		: 32*8 : string;
		Int32.of_int offset			: 32 : littleendian;
		Int32.of_int len			: 32 : littleendian
	}) in
	LevelDB.put storage.db ("txi_" ^ txhash) data;
;;

let get_tx storage txhash =
	match LevelDB.get storage.db ("txi_" ^ txhash) with
	| None -> None
	| Some (data) -> 
		let bdata = Bitstring.bitstring_of_string data in
		bitmatch bdata with
		| { 
			blockhash	    : 32*8 	: string;
			offset          : 32 	: littleendian;
			len 	        : 32 	: littleendian
		} ->
		let block = Hash.of_bin blockhash in
		match get_block storage block with
		| None -> None
		| Some (bdata) -> Some (Bytes.sub data (Int32.to_int offset) (Int32.to_int len))
;;


let get_header storage hash = 
	match LevelDB.get storage.db ("blk_" ^ hash) with
	| None -> None
	| Some (data) -> Some (Bytes.sub data 0 80)
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