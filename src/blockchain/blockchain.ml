open Block;;
open Tx;;
open Params;;
open Block;;
open Block.Header;;
open Hash;;
open Timediff;;
open Stdint;;
open Storage;;

module Resource = struct
	type t = 
	| RES_TXS of Tx.t list
	| RES_BLOCK of Block.t
	| RES_HBLOCKS of Block.Header.t list
	| RES_INV_TX of Hash.t * Unix.inet_addr
	| RES_INV_BLOCK of Hash.t * Unix.inet_addr
	;;
end

module Request = struct
	type t =
	| REQ_TXS of Hash.t list * Unix.inet_addr option
	| REQ_BLOCKS of Hash.t list * Unix.inet_addr option
	| REQ_HBLOCKS of Hash.t list * Unix.inet_addr option
	| REQ_DATA of Hash.t list * Unix.inet_addr option
	;;
end	

open Resource;;

type t = {
	params	: 	Params.t;
	basedir	:	string;
	
	storage :	Storage.t;

	(* Sync status *)
	mutable sync_headers	:	bool;
	mutable sync			:	bool;
	
	(* Last header status *)
	mutable header_height	:	int64;
	mutable header_last		: 	Header.t;

	(* Last block status *)
	mutable block_height 	:	int64;
	mutable block_last 		:	Block.t option;
	mutable block_last_received : float;
	
	mempool			:	(Hash.t, Tx.t) Hashtbl.t;
	
	(* Queue for incoming resources*)
	resources		:	(Resource.t) Cqueue.t;
	
	(* Queue for data request *)
	requests		:	(Request.t) Cqueue.t;
};;

let genesis path p = 
	let genesis_header : Block.Header.t = {
		hash		= p.genesis.hash;
		version		= p.genesis.version;
		prev_block	= p.genesis.prev_block;
		merkle_root = p.genesis.merkle_root;
		time		= p.genesis.time;
		bits		= p.genesis.bits;
		nonce		= p.genesis.nonce;
	} in
	let bc = {
		params			= p;
		basedir			= "";
		
		storage			= Storage.load path;
		sync_headers	= false;
		sync			= false;
		
		header_height	= 0L;
		header_last		= genesis_header;

		block_height 	= 0L;
		block_last 		= None;
		block_last_received = Unix.time ();
		
		mempool			= Hashtbl.create 4096;
		
		resources		= Cqueue.create ();
		requests		= Cqueue.create ();
	} in 
	bc
;;

let load path p = 
	let res bcg =
		Log.info "Blockchain" "Starting from block header %s at height %d" bcg.header_last.hash (Int64.to_int bcg.header_height);
		bcg
	in
	let bcg = genesis path p in
	if bcg.storage.chainstate.Chainstate.header <> "0000000000000000000000000000000000000000000000000000000000000000" 
	&& bcg.storage.chainstate.Chainstate.header_height <> Uint32.zero then (
		if bcg.storage.chainstate.Chainstate.block <> "0000000000000000000000000000000000000000000000000000000000000000" 
		&& bcg.storage.chainstate.Chainstate.height <> Uint32.zero then (
			match Storage.get_block bcg.storage bcg.storage.chainstate.Chainstate.block with
			| Some (bdata) -> (
				match Block.parse bdata with
				| None -> 				
					bcg.block_last <- None;
					bcg.block_height <- Uint32.to_int64 bcg.storage.chainstate.Chainstate.height
				| Some (block) ->
					bcg.block_last <- Some (block);
					bcg.block_height <- Uint32.to_int64 bcg.storage.chainstate.Chainstate.height
			)
			| None ->
				bcg.block_last <- None;
				bcg.block_height <- Uint32.to_int64 bcg.storage.chainstate.Chainstate.height
		) else (
			bcg.block_last <- None;
			bcg.block_height <- Uint32.to_int64 bcg.storage.chainstate.Chainstate.height
		);

		match Storage.get_header bcg.storage bcg.storage.chainstate.Chainstate.header with
		| Some (bdata) -> (
			match Block.Header.parse bdata with
			| Some (header) ->
				bcg.header_last <- header;
				bcg.header_height <- Uint32.to_int64 bcg.storage.chainstate.Chainstate.header_height;
				res bcg
			| None -> res bcg
		)
		| None -> res bcg
	) else res bcg
;;

(* Remove the last header / block (if detected a fork) *)
let revert_last bc =
	(match Storage.get_header bc.storage bc.header_last.prev_block with
	| None -> ()
	| Some (h) ->
		match Block.Header.parse h with
		| None -> ()
		| Some (h) ->
			Storage.remove_last_header bc.storage bc.header_last.hash;
			bc.header_last <- h;
			bc.header_height <- Int64.pred bc.header_height;
			Log.debug "Blockchain" "Reverting last header"
	);

	(match bc.block_last with
	| None -> ()
	| Some (b) -> 
		match Storage.get_block bc.storage b.header.prev_block with
		| None -> ()
		| Some (b) ->
			match Block.parse b with
			| None -> ()
			| Some (b) ->
				Storage.remove_last_block bc.storage bc.header_last.hash;
				bc.block_last <- Some (b);
				bc.block_height <- Int64.pred bc.block_height;
				Log.debug "Blockchain" "Reverting last block"
	);
	Storage.sync bc.storage
;;
	


let loop bc = 
	let rec consume () =
		let consume_block b = 
			match (b, bc.block_last, bc.header_last) with
			(* Genesis block *)
			| (b, None, hl) when b.header.hash = bc.params.genesis.hash ->
				bc.block_height <- Int64.zero;
				bc.block_last <- Some (b);
				Storage.insert_block bc.storage bc.block_height b.header.hash (Block.serialize b);
				consume ()
				
			(* Next block *)
			| (b, Some (block), hl) when b.header.prev_block = block.header.hash ->
				bc.block_height <- Int64.succ bc.block_height;
				bc.block_last <- Some (b);
				Storage.insert_block bc.storage bc.block_height b.header.hash (Block.serialize b);
				bc.block_last_received <- Unix.time ();

				let df = Timediff.diff (Unix.time ()) block.header.time in
				Log.debug "Blockchain ←" "Block %s - %d, time: %d y, %d m, %d d, %d h and %d m ago" block.header.hash (Int64.to_int bc.block_height) df.years df.months df.days df.hours df.minutes;
				consume ()
			
			(* New block *)
			| (b, Some (block), hl) when b.header.prev_block = hl.hash ->
				bc.header_last <- b.header;
				bc.header_height <- Int64.succ bc.header_height;

				let df = Timediff.diff (Unix.time ()) block.header.time in
				if b.header.prev_block = block.header.hash then (
					bc.block_height <- Int64.succ bc.block_height;
					bc.block_last <- Some (b);
					Storage.insert_block bc.storage bc.block_height b.header.hash (Block.serialize b);
					bc.block_last_received <- Unix.time ();
					Log.debug "Blockchain ←" "Block %s - %d, time: %d y, %d m, %d d, %d h and %d m ago" block.header.hash (Int64.to_int bc.block_height) df.years df.months df.days df.hours df.minutes;
				) else (
					Storage.insert_header bc.storage bc.header_height bc.header_last.hash (Block.Header.serialize bc.header_last);
					Log.debug "Blockchain ←" "Block header %s - %d, time: %d y, %d m, %d d, %d h and %d m ago" b.header.hash (Int64.to_int bc.block_height) df.years df.months df.days df.hours df.minutes
				);
				consume ()
			| _ ->
				consume ()
		in
		let rec consume_headers hl =
			match hl with
			| [] -> consume ()
			| h::hl' ->
				let check_checkpoint index hash =
					try
						let hash' = List.assoc index bc.params.checkpoints in
						Log.debug "Blockchain" "Checkpoint: %s" hash';
						hash' = hash
					with
					| _ -> true
				in
				if h.Header.prev_block = bc.header_last.Header.hash && check_checkpoint ((Int64.to_int bc.header_height) + 1) h.Header.hash then (
					(* Insert in the chain *)
					bc.header_last <- h;
					bc.header_height <- Int64.succ bc.header_height;
					Storage.insert_header bc.storage bc.header_height bc.header_last.hash (Block.Header.serialize bc.header_last);
					consume_headers hl'
				) else (
					consume_headers hl'
				)
		in

		if Cqueue.length bc.resources = 0 then ()
		else
			match Cqueue.get bc.resources with 
			| Some (res) -> (match (res : Resource.t) with 
				| RES_INV_BLOCK (bs, addr) -> 
					(if bc.sync then  Cqueue.add bc.requests @@ Request.REQ_BLOCKS ([bs], Some (addr)));
					consume ()
				| RES_INV_TX (txs, addr) -> consume ()
				| RES_BLOCK (bs) -> 
					(*let df = Timediff.diff (Unix.time ()) bs.header.time in
					Log.debug "Blockchain" "Got new block %s : %d y, %d m, %d d, %d h and %d m ago" bs.header.hash df.years df.months df.days df.hours df.minutes;*)
					consume_block (bs)
				| RES_TXS (txs) -> consume ()
				| RES_HBLOCKS (hbs) -> 
					if List.length hbs > 0 then (
						Log.debug "Blockchain ←" "Headers %d" (List.length hbs);

						(* Check if the list of headers is less than 1999; if yes, then verify to all nodes *)

						consume_headers (List.rev hbs)
					);
					consume ()
			)
			| None -> 
				consume ()
	in 

	
	while true do (
		Unix.sleep 4;
		Cqueue.clear bc.requests;
		
		(*revert_last bc;*)
		
		(* Handle new resources *)
		consume ();

		(* Check sync status *)
		if bc.header_last.time < (Unix.time () -. 60. *. 10.) then (
			let df = Timediff.diff (Unix.time ()) bc.header_last.time in
			Log.info "Blockchain" "Headers not in sync: %d years, %d months, %d days, %d hours and %d minutes behind" df.years df.months df.days df.hours df.minutes;
			bc.sync_headers <- false;
			Cqueue.add bc.requests @@ Request.REQ_HBLOCKS ([bc.header_last.hash], None);
		) else (
			let df = Timediff.diff (Unix.time ()) bc.header_last.time in
			Log.info "Blockchain" "Headers in sync: last block is %d years, %d months, %d days, %d hours and %d minutes" df.years df.months df.days df.hours df.minutes;
			bc.sync_headers <- true
		);

		(match bc.block_last with
		| None -> (
			Log.info "Blockchain" "Blocks not in sync, waiting for genesis";
			bc.sync <- false;
			Cqueue.add bc.requests @@ Request.REQ_BLOCKS ([bc.params.genesis.hash], None)
		)
		| Some (block) -> (
			if block.header.time < (Unix.time () -. 60. *. 10.) then (
				let df = Timediff.diff (Unix.time ()) block.header.time in
				Log.info "Blockchain" "Blocks not in sync: %d years, %d months, %d days, %d hours and %d minutes behind" df.years df.months df.days df.hours df.minutes;
				bc.sync <- false;

				(* Ask the storage for next n blocks hashes *)
				let rec getblockhashes h n acc = match n with
				| 0 -> acc
				| n ->
					let succ = Int64.succ h in
					let nh = Storage.get_headeri bc.storage succ in
					match nh with
					| None -> acc
					| Some (b) -> 
						let bh = Block.Header.parse b in
						match bh with 
						| Some (bh') -> getblockhashes succ (n-1) (bh'.hash::acc)
						| None -> acc
				in 
				if bc.block_last_received < (Unix.time () -. 3.) then (
					let hashes = getblockhashes (bc.block_height) 512 [] in
					Cqueue.add bc.requests @@ Request.REQ_BLOCKS (hashes, None)
				) else ()
			) else (
				let df = Timediff.diff (Unix.time ()) block.header.time in
				Log.info "Blockchain" "Blocks in sync: last block is %d years, %d months, %d days, %d hours and %d minutes" df.years df.months df.days df.hours df.minutes;
				bc.sync <- true
			)
		));


		Log.info "Blockchain" "Last block header is %d : %s" (Int64.to_int bc.header_height) bc.header_last.hash;
		(match bc.block_last with 
		| None -> ()
		| Some (b) -> Log.info "Blockchain" "Last block is %d : %s" (Int64.to_int bc.block_height) b.header.hash);
	) done
;;
