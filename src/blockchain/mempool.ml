open Bitcoinml;;

type t = {
          txs           : (Hash.t, Tx.t) Hashtbl.t;
  mutable fees          : Int64.t;
  mutable average_fee   : Int64.t;
  mutable size          : int;
};;

let empty () = {
  txs= Hashtbl.create 512;
  fees= Int64.zero;
  average_fee= Int64.zero;
  size= 0;
};;


let clear mp = 
  mp.fees <- Int64.zero;
  mp.average_fee= Int64.zero;
  mp.size <- 0;
  Hashtbl.reset mp.txs
;;


let add mp tx =
  Hashtbl.add mp.txs tx.Tx.hash tx;
  mp.size <- mp.size + tx.Tx.size;
  mp.fees <- Int64.add mp.fees @@ Tx.fee tx;
  mp.average_fee <- Int64.div (Int64.add mp.average_fee @@ Tx.fee tx) @@ Int64.of_int 2;
  true
;;