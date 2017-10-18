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
		mutable balance			: uint64;
		mutable sent			: uint64;
		mutable received		: uint64;
		mutable txs				: uint64;
		mutable utxs			: uint64;
	};;

	type utx = string * int * int64;;

	let parse data = 
		let bdata = Bitstring.bitstring_of_string data in
		match%bitstring bdata with
		| {|
			balance		: 64 	: string;
			sent			: 64 	: string;
			received	: 64	: string;
			txs				: 64	: string;
			utxs			: 64	: string
		|} ->
		{
			balance		= Uint64.of_bytes_little_endian balance 0;
			sent		= Uint64.of_bytes_little_endian sent 0;
			received	= Uint64.of_bytes_little_endian received 0;
			txs			= Uint64.of_bytes_little_endian txs 0;
			utxs		= Uint64.of_bytes_little_endian utxs 0;
		}
	;;

	let serialize addr = 
		let bs = [%bitstring {|
			Uint64.to_int64 addr.balance	: 64 : littleendian;
			Uint64.to_int64 addr.sent	  	: 64 : littleendian;
			Uint64.to_int64 addr.received	: 64 : littleendian;
			Uint64.to_int64 addr.txs	  	: 64 : littleendian;
			Uint64.to_int64 addr.utxs	  	: 64 : littleendian
		|}] in Bitstring.string_of_bitstring bs
	;;


	let add_tx batch addr txhash time =
		Batch.put batch ("adt_" ^ addr ^ string_of_float time ^ txhash) @@ 
			Bitstring.string_of_bitstring ([%bitstring {|
			Hash.to_bin (txhash)	: 32*8 : string
		|}])
	;;

	let remove_tx batch addr txhash time =
		Batch.delete batch ("adt_" ^ addr ^ string_of_float time ^ txhash)
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
		Batch.put batch ("adu_" ^ addr ^ txhash ^ string_of_int i) @@ 
			Bitstring.string_of_bitstring ([%bitstring {|
			Hash.to_bin (txhash)	: 32*8 : string;
			Int32.of_int i			: 32 : littleendian;
			value					: 64 : littleendian
		|}])
	;;

	let remove_utxo batch addr txhash i =
		Batch.delete batch ("adu_" ^ addr ^ txhash ^ string_of_int i)
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
			balance		= Uint64.zero;
			sent		= Uint64.zero;
			received	= Uint64.zero;
			txs			= Uint64.zero;
			utxs		= Uint64.zero;
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
			Hash.to_bin cs.block				: 32*8 : string;
			Uint32.to_int32 cs.height			: 32 : littleendian;
			Uint32.to_int32 cs.prune_height			: 32 : littleendian;
			Hash.to_bin cs.header             	: 32*8 : string;
			Uint32.to_int32 cs.header_height  	: 32 : littleendian;
			Uint64.to_int64 cs.txs			  	: 64 : littleendian;
			Uint64.to_int64 cs.utxos		  	: 64 : littleendian;
			Uint64.to_int64 cs.difficulty	  	: 64 : littleendian;
			Uint64.to_int64 cs.reward	  		: 64 : littleendian;
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
			block 		    : 32*8 	: string;
			height          : 32 	: string;
			prune_height          : 32 	: string;
			header 	        : 32*8 	: string;
			header_height	: 32 	: string;
			txs				: 64 	: string;
			utxos			: 64 	: string;
			difficulty		: 64 	: string;
			reward			: 64	: string;
			branches_n	: 32	: littleendian;
			rest : -1 : string
		|} ->
		{
			block 		    = Hash.of_bin block;
			height 	    	= Uint32.of_bytes_little_endian height 0;
			prune_height 	    	= Uint32.of_bytes_little_endian prune_height 0;
			header	    	= Hash.of_bin header;
			header_height	= Uint32.of_bytes_little_endian header_height 0;
			txs				= Uint64.of_bytes_little_endian txs 0;
			utxos			= Uint64.of_bytes_little_endian utxos 0;
			difficulty		= Uint64.of_bytes_little_endian difficulty 0;
			reward			= Uint64.of_bytes_little_endian reward 0;
			branches	= [] (*parse_branches rest (Int32.to_int branches_n) [];*)
		}
	;;
end

open Chainstate;;

type t = {
	chainstate		:	Chainstate.t;
	db       			: LevelDB.db;
	mutable batch : LevelDB.writebatch;
};;


let save_cs storage =
	let d = Chainstate.serialize storage.chainstate in
	Batch.put storage.batch "chainstate" d 
;;

let sync storage =
	Batch.write storage.db storage.batch;
	storage.batch <- LevelDB.Batch.make ();
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
	let db = LevelDB.open_db path in 
	{
		chainstate= load_cs db;
		db= db;
		batch= LevelDB.Batch.make ();
	}
;;

let close storage = 
	LevelDB.close storage.db
;;


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

	Batch.put storage.batch ("blk_" ^ header.hash) @@ Block.Header.serialize header;
	Batch.put storage.batch ("bih_" ^ header.hash) @@ Printf.sprintf "%d" (Uint32.to_int storage.chainstate.header_height);
	Batch.put storage.batch ("bli_" ^ Printf.sprintf "%d" (Uint32.to_int storage.chainstate.header_height)) header.hash;
	storage.chainstate.header <- header.hash;

	save_cs storage    
;;



let get_block_height storage hash =
	match LevelDB.get storage.db ("bih_" ^ hash) with
	| Some (hdata) -> int_of_string hdata
	| None -> 0
;;


let get_block storage hash = 
	if (get_block_height storage hash) > (Uint32.to_int storage.chainstate.height) then 
		None
	else
		match LevelDB.get storage.db ("blk_" ^ hash) with
		| Some (bdata) -> Block.parse bdata
		| None -> None
;;

let get_blocki storage height = 
	if (Int64.to_int height) > (Uint32.to_int storage.chainstate.height) then 
		None
	else
		match LevelDB.get storage.db ("bli_" ^ Printf.sprintf "%d" (Int64.to_int height)) with
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
				List.iter (fun tx -> Batch.delete storage.batch @@ "txi_" ^ tx.Tx.hash) block.txs;
				Batch.put storage.batch block.header.hash (Block.Header.serialize block.header);
				prune_blocks storage xb)
		| _ -> ()
	in

	Batch.put storage.batch ("blk_" ^ block.header.hash) @@ Block.serialize block;
	storage.chainstate.block <- block.header.hash;

	List.iteri (fun i tx -> 		
		(* Insert tx *)
		let data = Bitstring.string_of_bitstring ([%bitstring {|
			Hash.to_bin (block.header.hash)	: 32*8 : string;
			Int32.of_int i					: 32 : littleendian
		|}]) in
		Batch.put storage.batch ("txi_" ^ tx.Tx.hash) data;
		storage.chainstate.txs <- Uint64.add storage.chainstate.txs Uint64.one;

		(* Insert utxo and user utxo, set balances *)
		List.iteri (fun i out -> 
			if Tx.Out.is_spendable out then (
				Batch.put storage.batch ("utx_" ^ tx.Tx.hash ^ string_of_int i) @@ Tx.Out.serialize out;
				storage.chainstate.utxos <- Uint64.add storage.chainstate.utxos Uint64.one;

				(match Tx.Out.spendable_by out params.Params.prefixes with
				| None -> ()
				| Some (addr) -> 
					Address.add_utxo storage.batch addr tx.Tx.hash i out.value;
					Address.add_tx storage.batch addr tx.Tx.hash block.header.time;
						
					let addrd = Address.load_or_create storage.db addr in
					addrd.txs <- Uint64.add addrd.txs @@ Uint64.one;
					addrd.utxs <- Uint64.add addrd.utxs @@ Uint64.one;
					addrd.received <- Uint64.add addrd.received @@ Uint64.of_int64 out.value;
					addrd.balance <- Uint64.add addrd.balance @@ Uint64.of_int64 out.value;
					Address.save storage.batch addr addrd)
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
							(match Tx.Out.spendable_by utx params.Params.prefixes with
							| None -> ()
							| Some (addr) -> 
								Address.remove_utxo storage.batch addr ins.In.out_hash (Uint32.to_int ins.In.out_n);
								Address.add_tx storage.batch addr tx.Tx.hash block.header.time;

								let addrd = Address.load_or_create storage.db addr in
								addrd.txs <- Uint64.add addrd.txs @@ Uint64.one;
								addrd.sent <- Uint64.add addrd.sent @@ Uint64.of_int64 utx.value;
								addrd.balance <- Uint64.sub addrd.balance @@ Uint64.of_int64 utx.value;
								addrd.utxs <- Uint64.sub addrd.utxs @@ Uint64.one;
								Address.save storage.batch addr addrd
							);

							Batch.delete storage.batch key;
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

	save_cs storage;
	sync storage
;;


let get_utx	storage tx index =
	match LevelDB.get storage.db ("utx_" ^ tx ^ string_of_int index) with
	| None -> None
	| Some (data) -> 
		match Tx.Out.parse (Bitstring.bitstring_of_string data) with
		| (rest, txo) -> txo
;;

let get_tx storage txhash =
	match LevelDB.get storage.db ("txi_" ^ txhash) with
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
	match LevelDB.get storage.db ("txi_" ^ txhash) with
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

let get_address_utxs storage addr = 
	let a = get_address storage addr in
	Address.get_utxos storage.db addr (Uint64.to_int a.utxs)
;;


let get_address_txs storage addr = 
	let a = get_address storage addr in
	Address.get_txs storage.db addr (Uint64.to_int a.txs)
;;



let remove_last_header storage prevhash =
	storage.chainstate.header_height <- Uint32.sub (storage.chainstate.header_height) (Uint32.one);
	Batch.delete storage.batch ("bli_" ^ Printf.sprintf "%d" (Uint32.to_int storage.chainstate.header_height));
	Batch.delete storage.batch ("blk_" ^ storage.chainstate.header);
	Batch.delete storage.batch ("bih_" ^ storage.chainstate.header);	
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
			Batch.delete storage.batch ("txi_" ^ tx.Tx.hash);
			storage.chainstate.txs <- Uint64.sub storage.chainstate.txs Uint64.one;

			(* Remove utxo and user utxo, reset balances *)
			List.iteri (fun i out -> 
				if Tx.Out.is_spendable out then (
					Batch.delete storage.batch ("utx_" ^ tx.Tx.hash ^ string_of_int i);
					storage.chainstate.utxos <- Uint64.sub storage.chainstate.utxos Uint64.one;

					(match Tx.Out.spendable_by out params.Params.prefixes with
					| None -> ()
					| Some (addr) -> 
						Address.remove_utxo storage.batch addr tx.Tx.hash i;
						Address.remove_tx storage.batch addr tx.Tx.hash block.header.time;
							
						let addrd = Address.load_or_create storage.db addr in
						addrd.txs <- Uint64.sub addrd.txs @@ Uint64.one;
						addrd.utxs <- Uint64.sub addrd.utxs @@ Uint64.one;
						addrd.received <- Uint64.sub addrd.received @@ Uint64.of_int64 out.value;
						addrd.balance <- Uint64.sub addrd.balance @@ Uint64.of_int64 out.value;
						Address.save storage.batch addr addrd)
				)
			) tx.txout;

			(* Remove utxo and user utxo, set balances *)
			List.iter (fun ins -> 
				let utx = get_tx_output storage ins.In.out_hash @@ Uint32.to_int ins.In.out_n in
				let key = "utx_" ^ ins.In.out_hash ^ string_of_int (Uint32.to_int ins.In.out_n) in
				match utx with
				| None -> ()
				| Some (utx) -> 
					Batch.put storage.batch key @@ Tx.Out.serialize utx;

					if Tx.Out.is_spendable utx then (
						(match Tx.Out.spendable_by utx params.Params.prefixes with
						| None -> ()
						| Some (addr) -> 
							Address.add_utxo storage.batch addr ins.In.out_hash (Uint32.to_int ins.In.out_n);
							Address.remove_tx storage.batch addr tx.Tx.hash block.header.time;

							let addrd = Address.load_or_create storage.db addr in
							addrd.txs <- Uint64.sub addrd.txs @@ Uint64.one;
							addrd.sent <- Uint64.sub addrd.sent @@ Uint64.of_int64 utx.value;
							addrd.balance <- Uint64.add addrd.balance @@ Uint64.of_int64 utx.value;
							addrd.utxs <- Uint64.add addrd.utxs @@ Uint64.one;
							Address.save storage.batch addr addrd
						);

						storage.chainstate.utxos <- Uint64.add storage.chainstate.utxos Uint64.one
					)
			) tx.txin
		) block.txs;

		remove_last_header storage prevhash;
		sync storage
	))
;;