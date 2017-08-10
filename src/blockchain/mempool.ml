open Bitcoinml;;

type t = {
  txs     : (Hash.t, Tx.t) Hashtbl.t;
  fees    : Int64.t;
};;

let empty () = {
  txs= Hashtbl.create 512;
  fees= Int64.zero;
};;