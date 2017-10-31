open Stdint;;
open LevelDB;;
open Bitcoinml;;
open Block;;
open Block.Header;;
open Tx;;
open Utils;;
open Config;;

module Address = struct
	type t = {
		mutable balance		: int64;
		mutable sent			: int64;
		mutable received	: int64;
		mutable txs				: int64;
		mutable utxs			: int64;
	};;

	type utx = string * int * int64;;

	let parse data = 
		let bdata = Bitstring.bitstring_of_string data in
		match%bitstring bdata with
		| {|
			balance		: 64 	: littleendian;
			sent			: 64 	: littleendian;
			received	: 64	: littleendian;
			txs				: 64	: littleendian;
			utxs			: 64	: littleendian
		|} ->
		{
			balance		= balance;
			sent		= sent;
			received	= received;
			txs			= txs;
			utxs		= utxs;
		}
	;;

	let serialize addr = 
		let bs = [%bitstring {|
			addr.balance	: 64 : littleendian;
			addr.sent	  	: 64 : littleendian;
			addr.received	: 64 : littleendian;
			addr.txs	  	: 64 : littleendian;
			addr.utxs	  	: 64 : littleendian
		|}] in Bitstring.string_of_bitstring bs
	;;

	let add_tx batch addr txhash time =
		Batch.put batch ("adt_" ^ addr ^ string_of_float time ^ Hash.to_bin_norev txhash) @@ 
			Bitstring.string_of_bitstring ([%bitstring {|
			Hash.to_bin (txhash)	: 32*8 : string
		|}])
	;;

	let remove_tx batch addr txhash time =
		Batch.delete batch ("adt_" ^ addr ^ string_of_float time ^ Hash.to_bin_norev txhash)
	;;

	let get_txs db addr txs =
		let rec get_tx it n acc = match n with
		| 0 -> acc
		| n' -> match%bitstring Bitstring.bitstring_of_string @@ LevelDB.Iterator.get_value it with
			|  {| txhash		: 32*8 	: string |} -> 
				let _ = LevelDB.Iterator.next it in
				get_tx it (n' - 1) @@ (Hash.of_bin txhash) :: acc
		in
		let it = LevelDB.Iterator.make db in
		let _ = LevelDB.Iterator.seek it ("adt_" ^ addr) 0 (String.length @@ "adt_" ^ addr) in
		get_tx it txs []
	;;
	
	let add_utxo batch addr txhash i value =
		Batch.put batch ("adu_" ^ addr ^ Hash.to_bin_norev txhash ^ string_of_int i) @@ 
			Bitstring.string_of_bitstring ([%bitstring {|
			Hash.to_bin (txhash)	: 32*8 : string;
			Int32.of_int i			: 32 : littleendian;
			value					: 64 : littleendian
		|}])
	;;

	let remove_utxo batch addr txhash i =
		Batch.delete batch ("adu_" ^ addr ^ Hash.to_bin_norev txhash ^ string_of_int i)
	;;

	let get_utxos db addr utxs =
		let rec get_utx it n acc = match n with
		| 0 -> acc
		| n' -> match%bitstring Bitstring.bitstring_of_string @@ LevelDB.Iterator.get_value it with
			|  {|
				txhash	: 32*8 	: string;
				i				: 32 	: littleendian;
				value		: 64	: littleendian
			|} -> 
				let ut = (Hash.of_bin txhash, Int32.to_int i, value) in
				let _ = LevelDB.Iterator.next it in
				get_utx it (n' - 1) @@ ut :: acc
		in
		let it = LevelDB.Iterator.make db in
		let _ = LevelDB.Iterator.seek it ("adu_" ^ addr) 0 (String.length @@ "adu_" ^ addr) in
		get_utx it utxs []
	;;

	let load_or_create db addr =
		let empty = {
			balance		= Int64.zero;
			sent		= Int64.zero;
			received	= Int64.zero;
			txs			= Int64.zero;
			utxs		= Int64.zero;
		} in
		let key = "adr_" ^ addr in
		if LevelDB.mem db key then 
			match LevelDB.get db key with
			| Some (d) -> parse d
			| None -> empty
		else 
			empty
	;;

	let save batch addr data = 
		let key = "adr_" ^ addr in
		Batch.put batch key @@ serialize data
	;;
end


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

		mutable branches		: Branch.t list;
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
			Int32.of_int @@ List.length cs.branches : 32 : littleendian
		|}] ^
		List.fold_left (^) "" @@ List.map (fun b -> Branch.serialize b) cs.branches	
	;;

	let parse csb = 
		let rec parse_branches data n blist = if n = 0 then [] else
			match Branch.parse data with
			| (data', Some (b)) -> parse_branches data' (n-1) @@ blist @ [b]
			| (_, _) -> []
		in
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
			branches_n			: 32 : littleendian;
			rest 						: -1 : string
		|} ->
		{
			block 		    = Hash.of_bin block;
			height 	    	= Uint32.of_bytes_little_endian height 0;
			prune_height 	= Uint32.of_bytes_little_endian prune_height 0;
			header	    	= Hash.of_bin header;
			header_height	= Uint32.of_bytes_little_endian header_height 0;
			txs						= Uint64.of_bytes_little_endian txs 0;
			utxos					= Uint64.of_bytes_little_endian utxos 0;
			difficulty		= Uint64.of_bytes_little_endian difficulty 0;
			reward				= Uint64.of_bytes_little_endian reward 0;
			branches			= [] (*parse_branches rest (Int32.to_int branches_n) [];*)
		}
	;;
end

open Chainstate;;

type t = {
	chainstate			:	Chainstate.t;
	db_blocks 			: LevelDB.db;
	db_state  			: LevelDB.db;
	mutable batch_blocks : LevelDB.writebatch;
	mutable batch_state 	: LevelDB.writebatch;
};;



let save_cs storage =
	let d = Chainstate.serialize storage.chainstate in
	Batch.put storage.batch_state "chainstate" d 
;;

let sync storage =
	Batch.write storage.db_blocks storage.batch_blocks;
	Batch.write storage.db_state storage.batch_state;
	storage.batch_state <- LevelDB.Batch.make ();
	storage.batch_blocks <- LevelDB.Batch.make ();
	let d = Chainstate.serialize storage.chainstate in
	LevelDB.put ~sync:true storage.db_state "chainstate" d
;;


let load path =
	let load_cs db =
		match LevelDB.get db "chainstate" with
		| Some (d) -> Chainstate.parse d
		| None -> {
			block= "0000000000000000000000000000000000000000000000000000000000000000";
			height= Uint32.of_int 0;
			prune_height= Uint32.of_int 0;
			header= "0000000000000000000000000000000000000000000000000000000000000000";
			header_height= Uint32.of_int 0;

			txs= Uint64.of_int 0;
			utxos= Uint64.of_int 0;
			difficulty= Uint64.of_int 0;
			reward= Uint64.of_int 0;
			branches= [];
		}
	in
	let db_state = LevelDB.open_db (path ^ "/state") in 
	let db_blocks = LevelDB.open_db (path ^ "/blocks") in 
	{
		chainstate= load_cs db_state;
		db_state= db_state;
		db_blocks= db_blocks;
		batch_state= LevelDB.Batch.make ();
		batch_blocks= LevelDB.Batch.make ();
	}
;;

let close storage = 
	LevelDB.close storage.db_state;
	LevelDB.close storage.db_blocks
;;



module Addresscache = struct
	type t = (string, Address.t) Hashtbl.t;;

	let create () = Hashtbl.create 16;;

	let load address_tbl storage address =
		match Hashtbl.mem address_tbl address with
		| false -> 
			let addr = Address.load_or_create storage.db_state address in
			Hashtbl.add address_tbl address addr;
			addr
		| true -> Hashtbl.find address_tbl address
	;;

	let save address_tbl address addr =
		Hashtbl.replace address_tbl address addr
	;;

	let sync address_tbl storage =
		Hashtbl.iter (fun address addr -> 
			Address.save storage.batch_state address addr
		) address_tbl
	;;
end




let update_branches storage blist = 
	storage.chainstate.branches <- blist;
	save_cs storage
;;

let update_difficulty storage diff =
	storage.chainstate.difficulty <- diff;
	save_cs storage
;;

let update_reward storage reward =
	storage.chainstate.reward <- reward;
	save_cs storage
;;

let insert_header storage height (header : Block.Header.t) = 
	storage.chainstate.header_height <- Uint32.of_int64 height;

	Batch.put storage.batch_blocks ("blk_" ^ Hash.to_bin_norev header.hash) @@ Block.Header.serialize header;
	Batch.put storage.batch_blocks ("bih_" ^ Hash.to_bin_norev header.hash) @@ Printf.sprintf "%d" (Uint32.to_int storage.chainstate.header_height);
	Batch.put storage.batch_blocks ("bli_" ^ Printf.sprintf "%d" (Uint32.to_int storage.chainstate.header_height)) header.hash;
	storage.chainstate.header <- header.hash;

	save_cs storage    
;;



let get_block_height storage hash =
	match LevelDB.get storage.db_blocks ("bih_" ^ Hash.to_bin_norev hash) with
	| Some (hdata) -> int_of_string hdata
	| None -> 0
;;


let get_block storage hash = 
	if (get_block_height storage hash) > (Uint32.to_int storage.chainstate.height) then 
		None
	else
		match LevelDB.get storage.db_blocks ("blk_" ^ Hash.to_bin_norev hash) with
		| Some (bdata) -> Block.parse bdata
		| None -> None
;;

let get_blocki storage height = 
	if (Int64.to_int height) > (Uint32.to_int storage.chainstate.height) then 
		None
	else
		match LevelDB.get storage.db_blocks ("bli_" ^ Printf.sprintf "%d" (Int64.to_int height)) with
		| Some (h) -> get_block storage h 
		| None -> None
;;
	

let insert_block storage config params height (block : Block.t) = 
	let rec prune_blocks storage xb = 
		match (Uint32.to_int storage.chainstate.height) - xb with
		| x' when x' > Uint32.to_int storage.chainstate.prune_height -> (
			match get_blocki storage (Int64.of_uint32 storage.chainstate.prune_height) with
			| None -> 
				storage.chainstate.prune_height <- Uint32.add storage.chainstate.prune_height Uint32.one;
				prune_blocks storage xb
			| Some (block) ->
				Log.debug "Storage" "Pruned block %d (%d txs)" (Uint32.to_int storage.chainstate.prune_height) @@ List.length block.txs;
				storage.chainstate.prune_height <- Uint32.add storage.chainstate.prune_height Uint32.one;
				List.iter (fun tx -> Batch.delete storage.batch_blocks @@ "txi_" ^ tx.Tx.hash) block.txs;
				Batch.put storage.batch_blocks block.header.hash (Block.Header.serialize block.header);
				prune_blocks storage xb)
		| _ -> ()
	in

	(* Address caching, to handle many modification of the same address in the same block*)
	let addrcache = Addresscache.create () in

	(* Utx cache, so an output can be spent in the same block *)
	let utxcache = Hashtbl.create 16 in

	Batch.put storage.batch_blocks ("blk_" ^ Hash.to_bin_norev block.header.hash) @@ Block.serialize block;
	storage.chainstate.block <- block.header.hash;

	List.iteri (fun i tx -> 		
		(* Insert tx *)
		let data = Bitstring.string_of_bitstring ([%bitstring {|
			Hash.to_bin (block.header.hash)	: 32*8 : string;
			Int32.of_int i					: 32 : littleendian
		|}]) in
		Batch.put storage.batch_blocks ("txi_" ^ Hash.to_bin_norev tx.Tx.hash) data;
		storage.chainstate.txs <- Uint64.add storage.chainstate.txs Uint64.one;

		(* Insert utxo and user utxo, set balances *)
		List.iteri (fun i out -> 
			if Tx.Out.is_spendable out then (
				let utx_key = "utx_" ^ Hash.to_bin_norev tx.Tx.hash ^ string_of_int i in 
				let utx_ser = Tx.Out.serialize out in
				Batch.put storage.batch_state utx_key utx_ser;
				Hashtbl.add utxcache utx_key utx_ser;

				storage.chainstate.utxos <- Uint64.add storage.chainstate.utxos Uint64.one;

				(match Tx.Out.spendable_by out params.Params.prefixes with
				| None -> ()
				| Some (addr) -> 
					Address.add_utxo storage.batch_state addr tx.Tx.hash i out.value;
					Address.add_tx storage.batch_state addr tx.Tx.hash block.header.time;
						
					let addrd = Addresscache.load addrcache storage addr in
					addrd.txs <- Int64.succ addrd.txs;
					addrd.utxs <- Int64.succ addrd.utxs;
					addrd.received <- Int64.add addrd.received out.value;
					addrd.balance <- Int64.add addrd.balance out.value;
					Addresscache.save addrcache addr addrd
				)
			)
		) tx.txout;


		(* Get utx; if not present in the db, try to find it in the cache *)
		let get_utx key = 
			if LevelDB.mem storage.db_state key then (LevelDB.get storage.db_state key) 
			else if (Hashtbl.mem utxcache key) then (Some (Hashtbl.find utxcache key)) 
			else (None)
		in
		(* Remove utxo and user utxo, set balances *)
		List.iter (fun ins -> 
			let utx_key = "utx_" ^ Hash.to_bin_norev ins.In.out_hash ^ string_of_int (Uint32.to_int ins.In.out_n) in
			(match get_utx utx_key with
				| None -> ()
				| Some (utx) -> 
					let rest, utx = Tx.Out.parse @@ Bitstring.bitstring_of_string utx in
					match utx with
					| None -> ()
					| Some (utx) ->
						if Tx.Out.is_spendable utx then (
							(match Tx.Out.spendable_by utx params.Params.prefixes with
							| None -> ()
							| Some (addr) -> 
								Address.remove_utxo storage.batch_state addr ins.In.out_hash (Uint32.to_int ins.In.out_n);
								Address.add_tx storage.batch_state addr tx.Tx.hash block.header.time;

								let addrd = Addresscache.load addrcache storage addr in
								addrd.txs <- Int64.succ addrd.txs;
								addrd.sent <- Int64.add addrd.sent utx.value;
								addrd.balance <- Int64.sub addrd.balance utx.value;
								addrd.utxs <- Int64.prev addrd.utxs;
								Addresscache.save addrcache addr addrd
							);

							Hashtbl.remove utxcache utx_key;
							Batch.delete storage.batch_state utx_key;
							storage.chainstate.utxos <- Uint64.sub storage.chainstate.utxos Uint64.one
						);
			)
		) tx.txin;
	) block.txs;
	
	storage.chainstate.height <- Uint32.of_int64 height;


	(match config.mode with
	| PrunedNode (x) -> prune_blocks storage x
	| _ -> ()
	);

	Addresscache.sync addrcache storage;
	save_cs storage;
	sync storage
;;


let get_utx	storage tx index =
	match LevelDB.get storage.db_state ("utx_" ^ Hash.to_bin_norev tx ^ string_of_int index) with
	| None -> None
	| Some (data) -> 
		match Tx.Out.parse (Bitstring.bitstring_of_string data) with
		| (rest, txo) -> txo
		| _ -> None
;;

let get_tx storage txhash =
	match LevelDB.get storage.db_blocks ("txi_" ^ Hash.to_bin_norev txhash) with
	| None -> None
	| Some (data) -> 
		let bdata = Bitstring.bitstring_of_string data in
		match%bitstring bdata with
		| {| 
			blockhash  : 32*8 	: string;
			index      : 32 	: littleendian
		|} ->
		let block = Hash.of_bin blockhash in
		match get_block storage block with
		| None -> None
		| Some (b) -> Some (List.nth b.txs @@ Int32.to_int index)
;;


let get_tx_output storage tx index =
	match get_tx storage tx with
	| None -> None
	| Some (tx) -> 
		if List.length tx.txout >= index then
			Some (List.nth tx.txout index)
		else 
			None
;;

let get_tx_height storage txhash =
	match LevelDB.get storage.db_blocks ("txi_" ^ Hash.to_bin_norev txhash) with
	| None -> None
	| Some (data) -> 
		let bdata = Bitstring.bitstring_of_string data in
		match%bitstring bdata with
		| {| 
			blockhash  : 32*8 	: string;
			index      : 32 	: littleendian
		|} -> Some (get_block_height storage (Hash.of_bin blockhash))
;;


let get_header storage hash = 
	match LevelDB.get storage.db_blocks ("blk_" ^ Hash.to_bin_norev hash) with
	| None -> None
	| Some (data) -> Block.Header.parse @@ Bytes.sub data 0 80
;;

let get_headeri storage height = 
	match LevelDB.get storage.db_blocks ("bli_" ^ Printf.sprintf "%d" (Int64.to_int height)) with
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


let get_address storage addr = Address.load_or_create storage.db_state addr;;

let get_address_utxs storage addr = 
	let a = get_address storage addr in
	Address.get_utxos storage.db_state addr (Int64.to_int a.utxs)
;;


let get_address_txs storage addr = 
	let a = get_address storage addr in
	Address.get_txs storage.db_state addr (Int64.to_int a.txs)
;;



let remove_last_header storage prevhash =
	storage.chainstate.header_height <- Uint32.sub (storage.chainstate.header_height) (Uint32.one);
	Batch.delete storage.batch_blocks ("bli_" ^ Printf.sprintf "%d" (Uint32.to_int storage.chainstate.header_height));
	Batch.delete storage.batch_blocks ("blk_" ^ Hash.to_bin_norev storage.chainstate.header);
	Batch.delete storage.batch_blocks ("bih_" ^ Hash.to_bin_norev storage.chainstate.header);	
	storage.chainstate.header <- prevhash;

	save_cs storage;
	sync storage
;;

let remove_last_block storage config params prevhash =
	storage.chainstate.height <- Uint32.sub (storage.chainstate.height) (Uint32.one);
	storage.chainstate.block <- prevhash;

	(match get_block storage prevhash with
	| None -> failwith "impossible"
	| Some (block) -> (
		List.iteri (fun i tx -> 		
			Batch.delete storage.batch_blocks ("txi_" ^ Hash.to_bin_norev tx.Tx.hash);
			storage.chainstate.txs <- Uint64.sub storage.chainstate.txs Uint64.one;

			(* Remove utxo and user utxo, reset balances *)
			List.iteri (fun i out -> 
				if Tx.Out.is_spendable out then (
					Batch.delete storage.batch_state ("utx_" ^ Hash.to_bin_norev tx.Tx.hash ^ string_of_int i);
					storage.chainstate.utxos <- Uint64.sub storage.chainstate.utxos Uint64.one;

					(match Tx.Out.spendable_by out params.Params.prefixes with
					| None -> ()
					| Some (addr) -> 
						Address.remove_utxo storage.batch_state addr tx.Tx.hash i;
						Address.remove_tx storage.batch_blocks addr tx.Tx.hash block.header.time;
							
						let addrd = Address.load_or_create storage.db_state addr in
						addrd.txs <- Int64.prev addrd.txs;
						addrd.utxs <- Int64.prev addrd.utxs;
						addrd.received <- Int64.sub addrd.received out.value;
						addrd.balance <- Int64.sub addrd.balance out.value;
						Address.save storage.batch_state addr addrd)
				)
			) tx.txout;

			(* Remove utxo and user utxo, set balances *)
			List.iter (fun ins -> 
				let utx = get_tx_output storage ins.In.out_hash @@ Uint32.to_int ins.In.out_n in
				let key = "utx_" ^ Hash.to_bin_norev ins.In.out_hash ^ string_of_int (Uint32.to_int ins.In.out_n) in
				match utx with
				| None -> ()
				| Some (utx) -> 
					Batch.put storage.batch_state key @@ Tx.Out.serialize utx;

					if Tx.Out.is_spendable utx then (
						(match Tx.Out.spendable_by utx params.Params.prefixes with
						| None -> ()
						| Some (addr) -> 
							(* TODO: This add utxo has a wrong value *)
							Address.add_utxo storage.batch_state addr ins.In.out_hash (Uint32.to_int ins.In.out_n) Int64.zero;
							Address.remove_tx storage.batch_state addr tx.Tx.hash block.header.time;

							let addrd = Address.load_or_create storage.db_state addr in
							addrd.txs <- Int64.prev addrd.txs;
							addrd.sent <- Int64.sub addrd.sent utx.value;
							addrd.balance <- Int64.add addrd.balance utx.value;
							addrd.utxs <- Int64.succ addrd.utxs;
							Address.save storage.batch_state addr addrd
						);

						storage.chainstate.utxos <- Uint64.add storage.chainstate.utxos Uint64.one
					)
			) tx.txin
		) block.txs;

		remove_last_header storage prevhash;
		sync storage
	))
;;