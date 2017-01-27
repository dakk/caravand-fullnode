open Block;;
open Tx;;
open Params;;
open Block;;
open Timediff;;
open Stdint;;

module Resource = struct
	type t = 
	| RES_TXS of Tx.t list
	| RES_BLOCKS of Block.t list
	| RES_HBLOCKS of Block.Header.t list
	| RES_INV_TXS of Hash.t list * Unix.inet_addr
	| RES_INV_BLOCKS of Hash.t list * Unix.inet_addr
	| RES_INV_HBLOCKS of Hash.t list * Unix.inet_addr
	;;
end

module Request = struct
	type t =
	| REQ_TXS of Hash.t list * Unix.inet_addr option
	| REQ_BLOCKS of Hash.t list * Unix.inet_addr option
	| REQ_HBLOCKS of Hash.t list * Unix.inet_addr option
	;;
end	




type t = {
	params	: 	Params.t;
	basedir	:	string;
	
	storage :	Storage.t;

	(* Sync status *)
	mutable sync			:	bool;
	
	(* Last header status *)
	mutable header_height	:	int64;
	mutable header_last		: 	Header.t;

	(* Last block status *)
	(* mutable block_height 	:	int64;
	mutable block_last 		:	Block.t;*)
	
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
		sync			= false;
		
		header_height	= 0L;
		header_last		= genesis_header;

		(* block_height 	= 0L;
		block_last 		= None;
		block_timestamp	= 0.0;*)
		
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
	if bcg.storage.chainstate.header <> "" && bcg.storage.chainstate.header_height <> Uint32.zero then (
		match Storage.get_header bcg.storage bcg.storage.chainstate.header with
		| Some (bdata) -> (
			match Block.Header.parse bdata with
			| Some (header) ->
				bcg.header_last <- header;
				bcg.header_height <- Uint32.to_int64 bcg.storage.chainstate.header_height;
				res bcg
			| None -> res bcg
		)
		| None -> res bcg
	) else res bcg
;;




let loop bc = 
	let rec consume () =
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
				) else 
					consume_headers hl'
		in

		if Cqueue.len bc.resources = 0 then ()
		else
			match Cqueue.get bc.resources with 
			| Some (res) -> (match (res : Resource.t) with 
				| RES_INV_BLOCKS (bs, addr) -> consume ()
				| RES_INV_HBLOCKS (hbs, addr) -> consume ()
				| RES_INV_TXS (txs, addr) -> consume ()
				| RES_BLOCKS (bs) -> consume ()
				| RES_TXS (txs) -> consume ()
				| RES_HBLOCKS (hbs) -> 
					Log.info "Blockchain" "Got new %d headers" (List.length hbs);
					consume_headers (List.rev hbs)
			)
			| None -> 
				consume ()
	in 
	
	while true do (
		Unix.sleep 4;

		Log.info "Blockchain" "Height: %d, block: %s" (Int64.to_int bc.header_height) bc.header_last.hash;

		let reslen = Cqueue.len bc.resources in

		(* Check sync status *)
		if bc.header_last.time < (Unix.time () -. 60. *. 10.) then (
			let df = Timediff.diff (Unix.time ()) bc.header_last.time in
			Log.info "Blockchain" "Not in sync: %d years, %d months, %d days, %d hours and %d minutes behind" df.years df.months df.days df.hours df.minutes;
			bc.sync <- false
		) else (
			let df = Timediff.diff (Unix.time ()) bc.header_last.time in
			Log.info "Blockchain" "In sync: last block is %d years, %d months, %d days, %d hours and %d minutes" df.years df.months df.days df.hours df.minutes;
			bc.sync <- true
		);

		(* Handle new resources *)
		consume ();
		Storage.sync bc.storage;


		(* TODO Move request and response to new module *)
		(* Get in sync *)
		match bc.sync with 
		| false ->
			(*if bc.queue_req_last < (Unix.time () -. 10.) then*)
			Cqueue.clear bc.requests;
			Cqueue.add bc.requests (Request.REQ_HBLOCKS ([bc.header_last.hash], None))
			(*else 
				()*)
		| _ -> 
			()
	) done
;;

let sync bc = 
	()
;;

