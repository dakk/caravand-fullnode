open Block;;

module Resource : sig
	type t = 
	| RES_TXS of Tx.t list
	| RES_BLOCK of Block.t
	| RES_HBLOCKS of Block.Header.t list
	| RES_INV_TX of Hash.t * Unix.inet_addr
	| RES_INV_BLOCK of Hash.t * Unix.inet_addr
end

module Request : sig
	type t =
	| REQ_TXS of Hash.t list * Unix.inet_addr option
	| REQ_BLOCKS of Hash.t list * Unix.inet_addr option
	| REQ_HBLOCKS of Hash.t list * Unix.inet_addr option
	| REQ_DATA of Hash.t list * Unix.inet_addr option
end	


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
	mutable block_last 		:	Block.t;
	mutable block_last_received : float;
	mutable blocks_requested	:	int;
	
	mempool			:	(Hash.t, Tx.t) Hashtbl.t;
	
	(* Queue for incoming resources*)
	resources		:	(Resource.t) Cqueue.t;
	
	(* Queue for data request *)
	requests		:	(Request.t) Cqueue.t;
}


(* Load blockchain state *)
val load			: string -> Params.t -> t

(* Start the event loop for blockchain *)
val loop			: t -> unit
