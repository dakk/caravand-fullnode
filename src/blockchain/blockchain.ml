open Block;;
open Tx;;
open Params;;


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
	
	mutable height		:	int64;
	mutable last		:	Hash.t;
	mutable timestamp	:	float;
	
	mutable header_height	:	int64;
	mutable header_last		: 	Hash.t;
	mutable header_timestamp:	float;	
	
	mempool			:	(Hash.t, Tx.t) Hashtbl.t;
	
	(* Queue for incoming resources*)
	queue			:	(Resource.t) Queue.t;
	queue_lock		:	Mutex.t;
	
	(* Queue for data request *)
	queue_req		:	(Request.t) Queue.t; (* This should be a map for address*)
	queue_req_lock	:	Mutex.t;
};;

let genesis p = 
	let bc = {
		params			= p;
		basedir			= "";
		
		height			= 0L;
		last			= p.genesis;
		timestamp		= 0.0;
		
		header_height	= 0L;
		header_last		= p.genesis;
		header_timestamp= 0.0;
		
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


let loop bc = 
	while true do
		Unix.sleep 5;
		Log.debug "Blockchain" "height: %d, block: %s" (Int64.to_int bc.header_height) bc.header_last;
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
						if h.Header.prev_block = bc.header_last then (
							bc.header_last <- h.hash;
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

