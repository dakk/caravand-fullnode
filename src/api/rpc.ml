open Utils;;
open Bitcoinml;;
open Log;;
open Unix;;
open Blockchain;;
open Network;;
open Chain;;
open Block;;
open Block.Header;;
open Tx;;
open Tx.In;;
open Tx.Out;;
open Params;;
open Stdint;;


type t = {
	blockchain : Chain.t;
  network 	 : Net.t;
  conf       : Config.rpc;
	mutable run: bool;
};;

let init (rconf: Config.rpc) bc net = { blockchain= bc; conf= rconf; network= net; run = rconf.enable };;

let loop a =
  ()
;;

let shutdown a = 
	if a.conf.enable then (
    Log.fatal "Api.Rpc" "Shutdown...";
    a.run <- false;
  ) else ()
;;