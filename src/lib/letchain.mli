open Bitcoinml
open Utils
open Config

type t

val init : ?directory:string -> ?network:string -> ?peers:int -> ?loglevel:int -> node_type -> t
val stop : t -> bool

val is_synchronized : t -> bool
val get_height : t -> Int64.t
val get_last_block : t -> Block.t
val get_header_height : t -> Int64.t
val get_last_heaer : t -> Block.Header.t
val push_tx : t -> Tx.t -> bool
val get_utxo : t -> Hash.t -> int -> Tx.Out.t option