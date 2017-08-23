open Bitcoinml

type t

val create  : unit -> t
val add     : t -> Tx.t -> bool
val (<<)    : t -> Tx.t -> bool
val remove  : t -> Hash.t -> bool
val clear   : t -> unit