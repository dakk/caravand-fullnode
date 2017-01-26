open Stdint;;
open LevelDB;;

module Chainstate = struct
	type t = {
		lastblock : Hash.t;
		lastblockheight : uint32;

		lastblockheader : Hash.t;
		lastblockheaderheight : uint32;
	};;
end

type t = {
	chainstate		:	Chainstate.t;
    db              :   LevelDB.db;
};;

let load path =
    let db = LevelDB.open_db path in
    {
        chainstate= {
            lastblock= "";
            lastblockheight= Uint32.of_int 0;
            lastblockheader= "";
            lastblockheaderheight= Uint32.of_int 0;
        };
        db= db
    }
;;


let insert_blockheader db height hash header = ();;

let insert_block db height hash block = ();;

let get_blocki db height = None;;
let get_block db hash = None;;