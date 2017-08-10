open Bitcoinml;;

type t = (Hash.t, Tx.t) Hashtbl.t;;

let empty () = Hashtbl.create 512;;