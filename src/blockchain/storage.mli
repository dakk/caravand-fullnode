open Stdint
open LevelDB

module Chainstate : sig 
	type t = {
		lastblock : Hash.t;
		lastblockheight : uint32;

		lastblockheader : Hash.t;
		lastblockheaderheight : uint32;
	}
end

type t = {
	chainstate		:	Chainstate.t;
    db              :   LevelDB.db;
}


val load					:	string -> t

val insert_blockheader      :   t -> Uint64.t -> Hash.t -> Block.Header.t -> unit
val insert_block            :   t -> Uint64.t -> Hash.t -> Block.t -> unit

val get_blocki              :   t -> Uint64.t -> Block.t option
val get_block               :   t -> Hash.t -> Block.t option

