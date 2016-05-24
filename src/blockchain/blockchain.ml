open Params;;

module Resource = struct
	type t = 
	| RES_TXS of Tx.t list
	| RES_BLOCKS of Block.t list
	| RES_HBLOCKS of Block.Header.t list
	;;
end

module Request = struct
	type t =
	| REQ_TXS of Hash.t list
	| REQ_BLOCKS of Hash.t list
	| REQ_HBLOCKS of Hash.t list
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
	queue_req		:	(Request.t) Queue.t;
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
	Log.info "Blockchain" "Created genesis blockchain from block %s" (Hash.to_string p.genesis);
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
		Log.debug "Blockchain" "height: %d, block: %s" (Int64.to_int bc.header_height) (Hash.to_string bc.header_last);
		match get_resource bc with 
		| Some (res) -> (match res with 
			| RES_TXS (txs) ->
				()
			| RES_HBLOCKS (hbs) ->
				()
		)
		| None -> ();
	done
;;

let sync bc = 
	()
;;

