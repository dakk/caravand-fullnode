open Bitcoinml

type t

val empty : unit -> t
val add   : t -> Tx.t -> bool