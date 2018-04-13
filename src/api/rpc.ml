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
open Yojson.Basic.Util;;


let handle_request bc net req = 
	let reply = Helper.JSONRPC.reply req in

	Printf.printf "%s\n%!" req.methodn;
	match req.methodn with
	| "getblockcount" -> 
		let bl = Int64.to_int bc.block_height in
		reply (`Int bl)
	| "getblockhash" -> (
		match req.params with
		| [`String b] -> (
			match Storage.get_blocki bc.storage @@ Int64.of_string b with
			| None -> ()
			| Some (b) -> reply (`String b.header.hash)
		)
		| _ -> ()
	)
	| "getrawblock" -> (
		match req.params with
		| [`String b] -> (
			match Storage.get_block bc.storage b with
			| None -> ()
			| Some (b) -> reply (`String (Block.serialize b))
		)
		| _ -> ()
	)
	| _ -> ()
;;

type t = {
	blockchain : Chain.t;
  network 	 : Net.t;
  conf       : Config.rpc;
	mutable run: bool;
	socket		 : Unix.file_descr;
};;

let init (rconf: Config.rpc) bc net = { 
	blockchain= bc; 
	conf= rconf; 
	network= net; 
	run= rconf.enable;
	socket= socket PF_INET SOCK_STREAM 0
};;

let loop a =
	let rec do_listen socket =
		let (client_sock, _) = accept socket in
		match Helper.JSONRPC.parse_request client_sock with
		| None -> 
			(*close client_sock;*)
			if a.run then do_listen socket
		| Some (req) ->
			handle_request a.blockchain a.network req;
			(*close client_sock;*)
			if a.run then do_listen socket
	in
	if a.conf.enable then (
		Log.info "Api.Rpc" "Binding to port: %d" a.conf.port;
		Helper.listen a.socket a.conf.port 8;
    try do_listen a.socket with _ -> ()
	) else ()
;;

let shutdown a = 
	if a.conf.enable then (
    Log.fatal "Api.Rpc" "Shutdown...";
		Helper.shutdown a.socket;
		a.run <- false
  ) else ()
;;