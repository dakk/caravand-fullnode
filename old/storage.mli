open Stdint
open LevelDB
open Bitcoinml
open Tx
open Block
open Block.Header
open Utils


module Address : sig
	type t = {
		mutable balance		: int64;
		mutable sent			: int64;
		mutable received	: int64;
		mutable txs				: int64;
		mutable utxs			: int64;
	}

	type utx = string * int * int64

	val parse 					: bytes -> t
	val serialize 			: t -> bytes
	val load_or_create 	: LevelDB.db -> string -> t
	val save 						: LevelDB.writebatch -> string -> t -> unit
end

module Chainstate : sig 
	type t = {
		mutable block           : Hash.t;
		mutable height       		: uint32;
		mutable prune_height    : uint32;

		mutable header        	: Hash.t;
		mutable header_height 	: uint32;

		mutable txs				: uint64;
		mutable utxos			: uint64;

		mutable difficulty	: uint64;
		mutable reward 			: uint64;

		mutable branches		: Branch.t list;
		mutable address_index: bool;
	}

	val serialize	: 	t -> bytes
	val parse			: 	bytes -> t
end

type t = {
	chainstate		  :	Chainstate.t;
	db_blocks 			: LevelDB.db;
	db_state  			: LevelDB.db;
	db_address 			: LevelDB.db;
	mutable batch_blocks 	: LevelDB.writebatch;
	mutable batch_state 	: LevelDB.writebatch;
	mutable batch_address	: LevelDB.writebatch;
}


val load					:	string -> Config.t -> t
val close 				:	t -> unit
val sync					:	t -> unit 

val update_difficulty	: t -> uint64 -> unit
val update_reward			: t -> uint64 -> unit 

val update_branches		: t -> Branch.t list -> unit

(* Done *) 
val insert_header     : t -> int64 -> Block.Header.t -> unit
val remove_last_header: t -> Hash.t -> unit
val insert_block      : t -> Config.t -> Params.t -> int64 -> Block.t -> unit
val remove_last_block : t -> Config.t -> Params.t -> Hash.t -> unit

(* Done *)
val get_utx						:	t -> Hash.t -> int -> Tx.Out.t option
val get_blocki        : t -> Int64.t -> Block.t option
val get_block         : t -> Hash.t -> Block.t option
val get_block_height	:	t -> Hash.t -> int
val get_header				:	t -> Hash.t -> Block.Header.t option
val get_headeri				:	t -> Int64.t -> Block.Header.t option
val get_tx						:	t -> Hash.t -> Tx.t option
val get_blocks 				:	t -> Hash.t list -> Block.t list
val get_headers				:	t -> Hash.t list -> Block.Header.t list
val get_tx_output			:	t -> Hash.t -> int -> Tx.Out.t option

val get_tx_height			:	t -> Hash.t -> int option
val get_address				:	t -> string -> Address.t
val get_address_utxs	:	t -> string -> Address.utx list
val get_address_txs		:	t -> string -> string list

