open Bitcoinml

type t = {
  txs     : (Hash.t, Tx.t) Hashtbl.t;
  fees    : Int64.t;
}

val empty : unit -> t