type t = {
  best_branch : (Hash.t, Branch.t) option;
  branches    : (Hash.t, Branch.t) Hashtbl.t;
	db          : LevelDB.db;
};;

(* Return the number of active branches *)
let length bs = Hashtbl.length bs.branches;;

(* Load the storage state from path or return an empty object *)
let load path =
	let db = LevelDB.open_db path in {
    best_branch= None;
    branches= Hashtbl.create s;
		db= db;
	}
;;

(* Return the best branch (by height) *)
let get_best_branch bs = match bs.best_branch with
  | None -> None
  | Some (h, b) -> b
;;

(* Return the branch with the h last block *)
let get_branch_by_last bs h = Hashtbl.find bs.branches h;;


(* Update stale branches, if they are too old, remove them *)
let update_stale_branches bs = 
  0
;;


(* Push the blockheader in the correct branch *)
let push_header bs bh =

;;

(* Push the block in the correct branch *)
let push_block bs b =

;;