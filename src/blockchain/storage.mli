open Stdint
open LevelDB

module Chainstate : sig 
	type t = {
		mutable block               : Hash.t;
		mutable height       		: uint32;

		mutable header        : Hash.t;
		mutable header_height : uint32;
	}

	val serialize	: 	t -> bytes
	val parse		: 	bytes -> t
end

type t = {
	chainstate		:	Chainstate.t;
    db       		:   LevelDB.db;
}


val load					:	string -> t
val close 					:	t -> unit
val sync					:	t -> unit 

val insert_header      		:   t -> Int64.t -> Hash.t -> bytes -> unit
val insert_block            :   t -> Int64.t -> Hash.t -> bytes -> unit

val get_blocki              :   t -> Int64.t -> bytes option
val get_block               :   t -> Hash.t -> bytes option
val get_header				:	t -> Hash.t -> bytes option
val get_headeri				:	t -> Int64.t -> bytes option
val get_blocks 				:	t -> Hash.t list -> bytes list
val get_headers				:	t -> Hash.t list -> bytes list

