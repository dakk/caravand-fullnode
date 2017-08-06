type t = {
  best_branch : (Hash.t, Branch.t) option;
  branches    : (Hash.t, Branch.t) Hashtbl.t;
};;

(* Return the number of active branches *)
let length bs = Hashtbl.length bs.branches;;

(* Load the storage state from path or return an empty object *)
let load path =
  let init s = {
    best_branch= None;
    branches= Hashtbl.create s
  } in init 4
;;

(* Return the best branch (by height) *)
let get_best_branch bs = match bs.best_branch with
  | None -> None
  | Some (h, b) -> b
;;

(* Return the branch with the h last block *)
let get_branch_by_last bs h = Hashtbl.find bs.branches h;;

(* Push the blockheader in the correct branch *)
let push_header bs bh =
  
;;

(* Push the block in the correct branch *)
let push_block bs b =

;;