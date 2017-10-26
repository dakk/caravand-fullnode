open Utils;;
open Bitcoinml;;
open Tx;;
open Conv;;

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
  mp.average_fee <- Int64.zero;
  mp.size <- 0;
  Hashtbl.reset mp.txs
;;

let fees mp = mp.fees;;

let remove mp txhash = 
  match Hashtbl.mem mp.txs txhash with
  | false -> ()
  | true ->
    let tx = Hashtbl.find mp.txs txhash in
    let fee = fee_of_tx tx in
    mp.size <- mp.size - tx.size;
    mp.fees <- Int64.sub mp.fees fee;
    (* TODO: adjust average_fee *)
    Hashtbl.remove mp.txs txhash
;;

let get mp txhash = Hashtbl.find mp.txs txhash;;

let print_stats mp = 
  Log.debug "Mempool" "Size: %s, Txs: %d, Fees: %d" (byten_to_string mp.size) (Hashtbl.length mp.txs) (Int64.to_int @@ fees mp);
;;

let remove_txs mp txs = List.iter (fun tx -> remove mp tx.hash) txs;;

let has mp txh = if Hashtbl.mem mp.txs txh then true else false;;

let add mp tx =
  if has mp tx.Tx.hash then () else
  let fee = fee_of_tx tx in
  Hashtbl.add mp.txs tx.Tx.hash tx;
  mp.size <- mp.size + tx.Tx.size;
  mp.fees <- Int64.add mp.fees fee;
  mp.average_fee <- Int64.div (Int64.add mp.average_fee fee) @@ Int64.of_int 2
;;


let length mp = Hashtbl.length mp.txs;;

let size mp = mp.size;;

