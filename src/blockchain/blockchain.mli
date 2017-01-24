open Block;;

module Resource : sig
	type t = 
	| RES_TXS of Tx.t list
	| RES_BLOCKS of Block.t list
	| RES_HBLOCKS of Block.Header.t list
	| RES_INV_TXS of Hash.t list * Unix.inet_addr
	| RES_INV_BLOCKS of Hash.t list * Unix.inet_addr
	| RES_INV_HBLOCKS of Hash.t list * Unix.inet_addr
end

module Request : sig
	type t =
	| REQ_TXS of Hash.t list * Unix.inet_addr option
	| REQ_BLOCKS of Hash.t list * Unix.inet_addr option
	| REQ_HBLOCKS of Hash.t list * Unix.inet_addr option
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
