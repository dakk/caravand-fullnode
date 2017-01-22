open Block;;
open Tx;;
open Params;;
open Block;;

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
	
	(* Queue for data request *)
	queue_req		:	(Request.t) Queue.t; (* This should be a map for address*)
	queue_req_lock	:	Mutex.t;
};;

let genesis p = 
	let genesis_header : Block.Header.t = {
		hash		= p.genesis;
		version		= Int32.of_int 12;
		prev_block	= "00000000000000000";
		merkle_root = "00000000000000000";
		timestamp	= 12.0;
		bits		= Int32.of_int 1;
		nonce		= Int32.of_int 1;

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
		
		queue_req		= Queue.create ();
		queue_req_lock	= Mutex.create ();
	} in 
	Log.info "Blockchain" "Created genesis blockchain from block %s" p.genesis;
	bc
;;

let load p = genesis p;;




let add_resource bc r = 
	Mutex.lock bc.queue_lock;
	Queue.add r bc.queue;
	Mutex.unlock bc.queue_lock;
;;


let get_resource bc = 
	Mutex.lock bc.queue_lock;
	let r = if Queue.is_empty bc.queue then None else Some (Queue.take bc.queue) in
	Mutex.unlock bc.queue_lock;	
	r
;;


let add_request bc r = 
	Mutex.lock bc.queue_req_lock;
	Queue.add r bc.queue_req;
	Mutex.unlock bc.queue_req_lock;
;;


let get_request bc = 
	Mutex.lock bc.queue_req_lock;
	let r = if Queue.is_empty bc.queue_req then None else Some (Queue.take bc.queue_req) in
	Mutex.unlock bc.queue_req_lock;	
	r
;;



type timediff = {
	years 	: int;
	months	: int;
	days  	: int;
	minutes	: int;
};;

let time_diff a b = 
	let minutes = int_of_float ((a -. b) /. 60.) in
	let years = (minutes / 60 / 24 / 31 / 12) in
	let months = (minutes / 60 / 24 / 31) mod 12 in
	let days = (minutes / 60 / 24) mod 31 in
	let hours = (minutes / 60) mod 24 in
	let minutes = (minutes) mod 60 in
	{ years= years; months= months; days= days; minutes= minutes }
;;

let loop bc = 
	while true do
		Unix.sleep 5;

		Log.debug "Blockchain" "height: %d, block: %s" (Int64.to_int bc.header_height) bc.header_last.hash;

		(* Check sync status *)
		if bc.header_last.timestamp < (Unix.time () -. 60. *. 10.) then (
			let df = time_diff (Unix.time ()) bc.header_last.timestamp in
			Log.debug "Blockchain" "not in sync: %d years, %d months, %d days, %d minutes behind" df.years df.months df.days df.minutes;
			bc.sync <- false
		) else (
			Log.debug "Blockchain" "in sync";
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
				let rec h_header hl = 
					match hl with
					| [] -> ()
					| h::hl' ->
						if h.Header.prev_block = bc.header_last.hash then (
							bc.header_last <- h;
							bc.header_height <- Int64.succ bc.header_height
						) else 
							()
				in h_header (List.rev hbs)
		)
		| None -> ();
	done
;;

let sync bc = 
	()
;;

