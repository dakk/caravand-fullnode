open Bitcoinml

type t

val create  : unit -> t
val add     : t -> Tx.t -> unit
val has     : t -> Hash.t -> bool
val get     : t -> Hash.t -> Tx.t
val remove  : t -> Hash.t -> unit
val remove_txs : t -> Tx.t list -> unit
val clear   : t -> unit
val length  : t -> int
val size    : t -> int
val fees    : t -> Int64.t
val print_stats : t -> unit