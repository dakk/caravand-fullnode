open Bitcoinml;;

type t = {
          txs           : (Hash.t, Tx.t) Hashtbl.t;
  mutable fees          : Int64.t;
  mutable average_fee   : Int64.t;
  mutable size          : int;
};;

let fee_of_tx tx = 
  Int64.zero
;;

let create () = {
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
  let fee = fee_of_tx tx in
  Hashtbl.add mp.txs tx.Tx.hash tx;
  mp.size <- mp.size + tx.Tx.size;
  mp.fees <- Int64.add mp.fees fee;
  mp.average_fee <- Int64.div (Int64.add mp.average_fee fee) @@ Int64.of_int 2;
  true
;;