type t

val get_best_branch       : t -> Branch.t option
val get_branch_by_last    : t -> Hash.t -> Branch.t
val length                : t -> int

val push_block            : t -> Block.t -> Branch.t
val push_header           : t -> Block.Header.t -> Branch.t

val update_stale_branches : t -> int

val load                  : string -> t
val close                 : t -> bool
val sync                  : t -> bool

val get_blocks 				    :	t -> Hash.t list -> Block.t list
val get_headers				    :	t -> Hash.t list -> Block.Header.t list