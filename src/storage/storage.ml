open Store;;
open Utils;;
open Bitcoinml;;
open Stdint;;
open Block;;
open Block.Header;;
open Tx;;

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

module Chainstate_index = Store.Make_index 
  (Chainstate) 
  (struct let prefix = "chainstate" end)
;;

let load_or_init st (conf: Config.t) = 
  match Chainstate_index.get st "" with
    | Some (cs) -> 
      if cs.address_index <> conf.address_index then (
        failwith "Enabling / Disabling address index is allowed only on first run"
      ) else (cs)
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
			address_index= conf.address_index;
    }
;;



type t = {
	block_store: Store_raw.t;
	address_store: Store_raw.t;
	state_store: Store_raw.t;
	config: Config.t;
	mutable chainstate: Chainstate.t;
};;


let load path config = 
	let state_store = Store_raw.load path "state" in
	{
		chainstate= load_or_init state_store config;
		config= config;
		block_store= Store_raw.load path "blocks";
		address_store= Store_raw.load path "address";
		state_store= state_store
};;

let save st = Chainstate_index.set st.state_store "" st.chainstate;;

let sync st = 
  save st;
	Store_raw.sync st.block_store;
	Store_raw.sync st.address_store;
	Store_raw.sync st.state_store
;;

let close st = 
	sync st;
	Store_raw.close st.block_store;
	Store_raw.close st.address_store;
	Store_raw.close st.state_store
;;

	

let insert_header st height (header : Block.Header.t) = 
	let h = Uint32.of_int64 height in
	Blocks.insert_header st.block_store h header;
	
  st.chainstate.header <- header.hash;
	st.chainstate.header_height <- h;
  save st
;;

let remove_last_header st prevhash =
	st.chainstate.header_height <- Uint32.pred st.chainstate.header_height;
	st.chainstate.header <- prevhash;
	Blocks.remove_header st.block_store st.chainstate.header_height st.chainstate.header;
  save st;
	sync st
;;



let get_blocks st hashes = Blocks.get_blocks st.block_store hashes;;
let get_headers st hashes = Blocks.get_headers st.block_store hashes;;
let get_block_height st hash = Blocks.get_block_height st.block_store hash;;
let get_blocki st height = Blocks.get_blocki st.block_store height;;
let get_block st hash = Blocks.get_block st.block_store hash;;
let get_header st hash = Blocks.get_header st.block_store hash;;
let get_headeri st height = Blocks.get_headeri st.block_store height;;
let get_tx st txhash = Blocks.get_tx st.block_store txhash;;
let get_utx	st tx index = State.get_utx st.state_store tx index;;
let get_tx_output st tx index = Blocks.get_tx_output st.state_store tx index;;
let get_tx_height st txhash = Blocks.get_tx_height st.state_store txhash;;






let insert_block storage params height (block : Block.t) = 
	(*let rec prune_blocks storage xb = 
		match (Uint32.to_int storage.chainstate.height) - xb with
		| x' when x' > Uint32.to_int storage.chainstate.prune_height -> (
			match get_blocki storage (Int64.of_uint32 storage.chainstate.prune_height) with
			| None -> 
				storage.chainstate.prune_height <- Uint32.succ storage.chainstate.prune_height;
				prune_blocks storage xb
			| Some (block) ->
				let left = (Int64.to_int height) - (Uint32.to_int storage.chainstate.prune_height) - xb in
				Log.debug "Storage" "Pruned block %d (%d txs) - %d blocks left to prune" (Uint32.to_int storage.chainstate.prune_height) (List.length block.txs) left;
				storage.chainstate.prune_height <- Uint32.succ storage.chainstate.prune_height;
				List.iter (fun tx -> Batch.delete storage.batch_blocks @@ "tx" ^ Hash.to_bin_norev tx.Tx.hash) block.txs;
				(* Batch.delete storage.batch_blocks block.header.hash; useless, we overwrite this *)
				Batch.put storage.batch_blocks ("bk" ^ Hash.to_bin_norev block.header.hash) (Block.Header.serialize block.header);
				save_cs storage;
				sync storage;
				prune_blocks storage xb)
		| _ -> ()
	in*)

	(* Address caching, to handle many modification of the same address in the same block*)
	(*let addrcache = Addresscache.create () in*)

	(* Utx cache, so an output can be spent in the same block *)
	let utxcache = Hashtbl.create 16 in

	Blocks.insert_block (storage.block_store) block;
	storage.chainstate.block <- block.header.hash;

	List.iteri (fun i tx -> 		
		(* Insert tx *)
		Blocks.insert_tx storage.block_store tx.Tx.hash block.header.hash i;
		storage.chainstate.txs <- Uint64.succ storage.chainstate.txs;

		(* Insert utxo and user utxo, set balances *)
		List.iteri (fun i out -> 
			if Tx.Out.is_spendable out then (
				State.insert_utx storage.state_store tx.Tx.hash i out;
				Hashtbl.add utxcache (tx.Tx.hash ^ string_of_int i) out;

				storage.chainstate.utxos <- Uint64.succ storage.chainstate.utxos;

				(*if storage.chainstate.address_index then (
					match Tx.Out.spendable_by out params.Params.prefixes with
					| None -> ()
					| Some (addr) -> 
						Address.add_utxo storage.batch_address addr tx.Tx.hash i out.value;
						Address.add_tx storage.batch_address addr tx.Tx.hash block.header.time;
							
						let addrd = Addresscache.load addrcache storage addr in
						addrd.txs <- Int64.succ addrd.txs;
						addrd.utxs <- Int64.succ addrd.utxs;
						addrd.received <- Int64.add addrd.received out.value;
						addrd.balance <- Int64.add addrd.balance out.value;
						Addresscache.save addrcache addr addrd
				)*)
			)
		) tx.txout;


		(* Get utx; if not present in the db, try to find it in the cache *)
		let get_utx hash i = 
			match State.get_utx storage.state_store hash i with
			| Some (utx) -> Some (utx)
			| None ->
				try Some (Hashtbl.find utxcache @@ hash ^ string_of_int i)
				with _ -> None
		in
		(* Remove utxo and user utxo, set balances *)
		List.iter (fun ins -> 
			(match get_utx ins.In.out_hash (Uint32.to_int ins.In.out_n) with
				| None -> ()
				| Some (utx) -> 
					if Tx.Out.is_spendable utx then (
						(*if storage.chainstate.address_index then (
							match Tx.Out.spendable_by utx params.Params.prefixes with
							| None -> ()
							| Some (addr) -> 
								Address.remove_utxo storage.batch_address addr ins.In.out_hash (Uint32.to_int ins.In.out_n);
								Address.add_tx storage.batch_address addr tx.Tx.hash block.header.time;

								let addrd = Addresscache.load addrcache storage addr in
								addrd.txs <- Int64.succ addrd.txs;
								addrd.sent <- Int64.add addrd.sent utx.value;
								addrd.balance <- Int64.sub addrd.balance utx.value;
								addrd.utxs <- Int64.pred addrd.utxs;
								Addresscache.save addrcache addr addrd
						);*)

						Hashtbl.remove utxcache (ins.In.out_hash ^ string_of_int (Uint32.to_int ins.In.out_n));
						State.remove_utx storage.state_store ins.In.out_hash (Uint32.to_int ins.In.out_n);
						storage.chainstate.utxos <- Uint64.pred storage.chainstate.utxos
					);
			)
		) tx.txin;
	) block.txs;
	
	storage.chainstate.height <- Uint32.of_int64 height;


	(*(match storage.config.mode with
	| PrunedNode (x) -> prune_blocks storage x
	| _ -> ()
	);

	if storage.chainstate.address_index then (
		Addresscache.sync addrcache storage
	);*)
  Chainstate_index.set storage.state_store "" storage.chainstate;
	sync storage
;;



let remove_last_block storage params prevhash =
	storage.chainstate.height <- Uint32.pred storage.chainstate.height;
	storage.chainstate.block <- prevhash;

	(match get_block storage prevhash with
	| None -> failwith "impossible"
	| Some (block) -> (
		List.iteri (fun i tx -> 		
			Blocks.remove_tx storage.block_store tx.Tx.hash;
			storage.chainstate.txs <- Uint64.pred storage.chainstate.txs;

			(* Remove utxo and user utxo, reset balances *)
			List.iteri (fun i out -> 
				if Tx.Out.is_spendable out then (
					State.remove_utx storage.state_store tx.Tx.hash i;
					storage.chainstate.utxos <- Uint64.pred storage.chainstate.utxos;

					(*if storage.chainstate.address_index then (
						match Tx.Out.spendable_by out params.Params.prefixes with
						| None -> ()
						| Some (addr) -> 
							Address.remove_utxo storage.batch_address addr tx.Tx.hash i;
							Address.remove_tx storage.batch_address addr tx.Tx.hash block.header.time;
								
							let addrd = Address.load_or_create storage.db_address addr in
							addrd.txs <- Int64.pred addrd.txs;
							addrd.utxs <- Int64.pred addrd.utxs;
							addrd.received <- Int64.sub addrd.received out.value;
							addrd.balance <- Int64.sub addrd.balance out.value;
							Address.save storage.batch_address addr addrd
					)*)
				)
			) tx.txout;

			(* Remove utxo and user utxo, set balances *)
			List.iter (fun ins -> 
				let utx = 
					Blocks.get_tx_output storage.block_store ins.In.out_hash @@ Uint32.to_int ins.In.out_n in
				match utx with
				| None -> ()
				| Some (utx) -> 
					State.insert_utx storage.state_store ins.In.out_hash (Uint32.to_int ins.In.out_n) utx;

					if Tx.Out.is_spendable utx then (
						(*if storage.chainstate.address_index then (
							match Tx.Out.spendable_by utx params.Params.prefixes with
							| None -> ()
							| Some (addr) -> 
								(* TODO: This add utxo has a wrong value *)
								Address.add_utxo storage.batch_address addr ins.In.out_hash (Uint32.to_int ins.In.out_n) Int64.zero;
								Address.remove_tx storage.batch_address addr tx.Tx.hash block.header.time;

								let addrd = Address.load_or_create storage.db_address addr in
								addrd.txs <- Int64.pred addrd.txs;
								addrd.sent <- Int64.sub addrd.sent utx.value;
								addrd.balance <- Int64.add addrd.balance utx.value;
								addrd.utxs <- Int64.succ addrd.utxs;
								Address.save storage.batch_address addr addrd
						);*)

						storage.chainstate.utxos <- Uint64.succ storage.chainstate.utxos
					)
			) tx.txin
		) block.txs;

		remove_last_header storage prevhash;
		sync storage
	))
;;