module Resource : sig
	type t = 
	| RES_TXS of (Tx.t * Hash.t) list
	| RES_BLOCKS of (Block.t * Hash.t) list
	| RES_HBLOCKS of (Block.Header.t * Hash.t) list
end

module Request : sig
	type t =
	| REQ_TXS of Hash.t list
	| REQ_BLOCKS of Hash.t list
	| REQ_HBLOCKS of Hash.t list
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
}


(* Load blockchain state *)
val load			: Params.t -> t

(* Create the genesis blockchain state *)
val genesis 		: Params.t -> t

(* Start the event loop for blockchain *)
val loop			: t -> unit

(* Start sync *)
val sync			: t -> unit


(* Thread-safe resource add: used by network *)
val add_resource 	: t -> Resource.t -> unit

(* Thread-safe resource get: used by blockchain *)
val get_resource	: t -> Resource.t option

(* Thread-safe request add: used by blockchain *)
val add_request		: t -> Request.t -> unit

(* Thread-safe request get: used by network *)
val get_request		: t -> Request.t option
