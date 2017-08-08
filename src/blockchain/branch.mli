open Bitcoinml

type t = {
  storage_prefix        : string;

  (* Fork status *)
	fork_hash	    :	Hash.t;
  fork_height	  :	int64;

	(* Last header status *)
	mutable header_height	:	int64;
  mutable header_last		: Block.Header.t;

  mutable header_list   : Block.Header.t list;
}

val create      : Hash.t -> int64 -> Block.Header.t -> t
val last        : t -> Hash.t
val push        : t -> Block.Header.t -> bool

(* Or deriving sexp? *)
val serialize   : t -> bytes
val parse       : bytes -> t