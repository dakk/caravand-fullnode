open Bitcoinml

type t = (Hash.t, Tx.t) Hashtbl.t

val empty : unit -> t