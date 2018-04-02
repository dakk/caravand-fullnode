open Bitcoinml
open Block
open Utils


(* Data verification *)
val verify_tx           : Params.t -> Tx.t -> bool
val verify_txs          : Params.t -> Tx.t list -> bool
val verify_block        : ?verifyheader:bool -> Params.t -> Int64.t -> Block.t -> Block.t -> bool
val verify_block_header : Params.t -> Int64.t -> Block.Header.t -> Block.Header.t -> bool