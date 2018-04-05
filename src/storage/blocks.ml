open Utils;;
open LevelDB;;
open Bitcoinml;;
open Stdint;;
open Store;;

type t = Store_raw.t;;

module Block_index = Store.Make_index 
  (Block)
  (struct let prefix = "bk" end)
;;

module Block_header_index = Store.Make_index 
  (Block.Header)
  (struct let prefix = "bk" end)
;;


module Block_height_index = Store.Make_index 
  (struct 
    type t = string;;
    let serialize o = o;;
    let parse o = Some o;;
  end) 
  (struct let prefix = "bi" end)
;;

module Block_height_index_reverse = Store.Make_index 
  (struct 
    type t = Uint32.t;;
    let serialize o = Printf.sprintf "%d" @@ Uint32.to_int o;;
    let parse o = Some (Uint32.of_int @@ int_of_string o);;
  end) 
  (struct let prefix = "bh" end)
;;


module Tx_index = Store.Make_index 
  (struct 
    type t = Hash.t * int;;
    
    let serialize tob = 
      let hash = fst tob in
      let index = snd tob in 
      Bitstring.string_of_bitstring ([%bitstring {|
        Hash.to_bin hash	: 32*8 : string;
        Int32.of_int index: 32 : littleendian
      |}])
    ;;
        
    let parse data = 
      let bdata = Bitstring.bitstring_of_string data in
      match%bitstring bdata with
      | {| 
        blockhash  : 32*8 	: string;
        index      : 32 	: littleendian
      |} -> Some (Hash.of_bin blockhash, Int32.to_int index)
    ;;
  end) 
  (struct let prefix = "ut" end)
;;

let insert_header block_store height (header : Block.Header.t) = 
	Block_header_index.set block_store (Hash.to_bin_norev header.hash) header;
	Block_height_index_reverse.set block_store (Hash.to_bin_norev header.hash) height;
	Block_height_index.set block_store (Printf.sprintf "%d" (Uint32.to_int height)) header.hash;
;;


let insert_block block_store (block : Block.t) = 
  Block_index.set block_store (Hash.to_bin_norev block.header.hash) block
;;

let insert_tx block_store txhash hash i = 
  Tx_index.set block_store (Hash.to_bin_norev txhash) (hash, i)
;;

let remove_tx block_store txhash =
  Tx_index.remove block_store txhash
;;


let remove_header block_store height hash = 
  Block_height_index.remove block_store @@ Printf.sprintf "%d" (Uint32.to_int height);
	Block_height_index_reverse.remove block_store @@ Hash.to_bin_norev hash;
	Block_header_index.remove block_store @@ Hash.to_bin_norev hash;	
;;

let get_block_height block_store hash =
	match Block_height_index_reverse.get block_store (Hash.to_bin_norev hash) with
	| Some (d) -> Uint32.to_int d
	| None -> 0
;;

let get_block block_store hash = Block_index.get block_store @@ Hash.to_bin_norev hash;;

let get_blocki block_store height = 
  match Block_height_index.get block_store @@ Printf.sprintf "%d" (Int64.to_int height) with
  | Some (h) -> get_block block_store h 
  | None -> None
;;

let get_header block_store hash = Block_header_index.get block_store @@ Hash.to_bin_norev hash;;

let get_headeri block_store height = 
  match Block_height_index.get block_store @@ Printf.sprintf "%d" (Int64.to_int height) with
  | Some (h) -> get_header block_store h 
  | None -> None
;;

let get_tx block_store txhash =
	match Tx_index.get block_store @@ Hash.to_bin_norev txhash with
	| None -> None
	| Some (block, index) -> 
		match get_block block_store block with
		| None -> None
		| Some (b) -> Some (List.nth b.txs index)
;;


let get_tx_output block_store txhash index =
	match get_tx block_store txhash with
	| None -> None
	| Some (tx) -> 
		if List.length tx.txout >= index then
			Some (List.nth tx.txout index)
		else 
			None
;;

let get_tx_height block_store txhash =
	match Tx_index.get block_store @@ Hash.to_bin_norev txhash with
	| None -> None
	| Some (block, index) -> 
    Some (get_block_height block_store block)
;;

let get_blocks block_store hashes = 
	let rec get_blocks' hs acc = match hashes with
	| [] -> acc
	| h::hs' -> match get_block block_store h with
		| None -> get_blocks' hs' acc
		| Some (h') -> get_blocks' hs' (h'::acc)
	in get_blocks' hashes []
;;

let get_headers block_store hashes = 
	let rec get_headers' hs acc = match hashes with
	| [] -> acc
	| h::hs' -> match get_header block_store h with
		| None -> get_headers' hs' acc
		| Some (h') -> get_headers' hs' (h'::acc)
	in get_headers' hashes []
;;