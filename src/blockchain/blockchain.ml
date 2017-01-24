open Block;;
open Tx;;
open Params;;
open Block;;
open Timediff;;

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
	queue			:	(Resource.t) Queue.t;
	queue_lock		:	Mutex.t;
	mutable queue_last		:	float;
	
	(* Queue for data request *)
	queue_req		:	(Request.t) Queue.t; (* This should be a map for address*)
	queue_req_lock	:	Mutex.t;
	mutable queue_req_last	:	float;
};;

let genesis p = 
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
		
		sync			= false;
		
		header_height	= 0L;
		header_last		= genesis_header;

		(* block_height 	= 0L;
		block_last 		= None;
		block_timestamp	= 0.0;*)
		
		mempool			= Hashtbl.create 4096;
		
		queue			= Queue.create ();
		queue_lock		= Mutex.create ();
		queue_last		= Unix.time ();
		
		queue_req		= Queue.create ();
		queue_req_lock	= Mutex.create ();
		queue_req_last	= Unix.time ();
	} in 
	Log.info "Blockchain" "Created genesis blockchain from block %s" p.genesis.hash;
	bc
;;

let load p = genesis p;;




let add_resource bc r = 
	Mutex.lock bc.queue_lock;
	Queue.add r bc.queue;
	bc.queue_last <- Unix.time ();
	Mutex.unlock bc.queue_lock;
;;


let get_resource bc = 
	Mutex.lock bc.queue_lock;
	let r = if Queue.is_empty bc.queue then None else Some (Queue.take bc.queue) in
	Mutex.unlock bc.queue_lock;	
	r
;;

let resource_length bc = 
	Mutex.lock bc.queue_lock;
	let r = Queue.length bc.queue in
	Mutex.unlock bc.queue_lock;	
	r
;;

let add_request bc r = 
	Mutex.lock bc.queue_req_lock;
	Queue.add r bc.queue_req;
	bc.queue_req_last <- Unix.time ();
	Mutex.unlock bc.queue_req_lock;
;;


let get_request bc = 
	Mutex.lock bc.queue_req_lock;
	let r = if Queue.is_empty bc.queue_req then None else Some (Queue.take bc.queue_req) in
	Mutex.unlock bc.queue_req_lock;	
	r
;;



let loop bc = 
	while true do (
		Unix.sleep 4;

		Log.debug "Blockchain" "height: %d, block: %s" (Int64.to_int bc.header_height) bc.header_last.hash;

		let reslen = resource_length bc in


		(* Check sync status *)
		if bc.header_last.time < (Unix.time () -. 60. *. 10.) then (
			let df = Timediff.diff (Unix.time ()) bc.header_last.time in
			Log.debug "Blockchain" "not in sync: %d years, %d months, %d days, %d hours and %d minutes behind" df.years df.months df.days df.hours df.minutes;
			bc.sync <- false
		) else (
			let df = Timediff.diff (Unix.time ()) bc.header_last.time in
			Log.debug "Blockchain" "in sync: last block is %d years, %d months, %d days, %d hours and %d minutes" df.years df.months df.days df.hours df.minutes;
			bc.sync <- true
		);

		(* Handle new resources *)
		match get_resource bc with 
		| Some (res) -> (match res with 
			| RES_INV_BLOCKS (bs, addr) -> ()
			| RES_INV_HBLOCKS (hbs, addr) -> ()
			| RES_INV_TXS (txs, addr) -> ()
			| RES_BLOCKS (bs) -> ()
			| RES_TXS (txs) -> ()
			| RES_HBLOCKS (hbs) -> 
				Log.debug "Blockchain" "Got new header blocks %d" (List.length hbs);
				let rec h_header hl = 
					match hl with
					| [] -> ()
					| h::hl' ->
						if h.Header.prev_block = bc.header_last.hash then (
							bc.header_last <- h;
							bc.header_height <- Int64.succ bc.header_height;
							h_header hl'
						) else 
							h_header hl'
				in h_header (List.rev hbs)
		)
		| None -> ();

		(* TODO Move request and response to new module *)
		(* Get in sync *)
		match bc.sync with 
		| false ->
			(*if bc.queue_req_last < (Unix.time () -. 10.) then*)
			Log.debug "Blockchain" "Asking for headers";
				add_request bc (Request.REQ_HBLOCKS ([bc.header_last.hash], None))
			(*else 
				()*)
		| _ -> 
			()
	) done
;;

let sync bc = 
	()
;;

