open Bitcoinml
open Block
open Utils

(*
type peerdest = None | Some of Unix.inet_addr | Broadcast
*)

module QueueMessage : sig
	type t = 
	| RES_TXS of Tx.t list
	| RES_BLOCK of Block.t
	| RES_HBLOCKS of Block.Header.t list * Unix.inet_addr
	| RES_INV_TX of Hash.t * Unix.inet_addr
	| RES_INV_BLOCK of Hash.t * Unix.inet_addr
<<<<<<< HEAD
	| REQ_HBLOCKS of Hash.t list * Hash.t * Unix.inet_addr
end

module Request : sig
	type t =
=======
>>>>>>> 6acee1281950c0dfd420d950ab1e1e9d96bc3a8c
	| REQ_TXS of Hash.t list * Unix.inet_addr option
	| REQ_BLOCKS of Hash.t list * Unix.inet_addr option
	| REQ_HBLOCKS of Hash.t list * Unix.inet_addr option
	| REQ_DATA of Hash.t list * Unix.inet_addr option
	| RES_HBLOCKS of Block.Header.t list * Unix.inet_addr
end	


type t = {
	params	: Params.t;
	basedir	:	string;

	storage :	Storage.t;
	
	(* Sync status *)
	mutable sync_headers	:	bool;
	mutable sync					:	bool;

	(* Branches *)
	mutable branches			: Branch.t list;
	
	(* Last header status *)
	mutable header_height	:	int64;
	mutable header_last		: 	Header.t;

	(* Last block status *)
	mutable block_height 	:	int64;
	mutable block_last 		:	Block.t;
	mutable block_last_received : float;
	mutable blocks_requested	:	int;
	
	mempool			:	Mempool.t;
	
	(* Queue for incoming resources*)
	resources		:	(QueueMessage.t) Cqueue.t;
	
	(* Queue for data request *)
	requests		:	(QueueMessage.t) Cqueue.t;
}

(* Data verification *)
val verify_tx           : t -> Tx.t -> bool
val verify_txs          : t -> Tx.t list -> bool
val verify_block        : t -> Block.t -> bool
val verify_block_header : t -> Block.Header.t -> bool

(* Load blockchain state *)
val load			: string -> Params.t -> t

(* Start the event loop for blockchain *)
val loop			: t -> unit
