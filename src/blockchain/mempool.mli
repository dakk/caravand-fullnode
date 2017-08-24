open Bitcoinml

type t

val create  : unit -> t
val add     : t -> Tx.t -> bool
val has     : t -> Hash.t -> bool
val (<<)    : t -> Tx.t -> bool
val remove  : t -> Hash.t -> bool
val clear   : t -> unit
val length  : t -> int
val size    : t -> int
val fees    : t -> Int64.t