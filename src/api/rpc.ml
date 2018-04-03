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

module Request = struct
	type t = {
    methodn : string;
    params  : Yojson.Basic.json list;
    id      : string;
		socket 	: Unix.file_descr;
	};;

	let recv socket = 
		try (
			let data_raw = Bytes.create 4096 in
			let reqlen = Unix.recv socket data_raw 0 4096 [] in
			let data_raw = String.sub data_raw 0 reqlen in
			let data_split = String.split_on_char '{' data_raw in
			let body = "{" ^ List.nth data_split 1 in
			let j = Yojson.Basic.from_string body in
			Some ({
				socket= socket;
				id= j |> member "id" |> to_string;
				params= []; (*j |> member "params" |> to_list;*)
				methodn= j |> member "method" |> to_string;
			})
		) with _ -> None
	;;

	let reply req jdata =
		let send_string str = 
			let len = String.length str in
			send req.socket str 0 len [] |> ignore
		in 
		`Assoc [
			("id", `String req.id);
			("result", jdata);
		] |> to_string |> send_string
	;;

	let replyerr req jdata =
		let send_string str = 
			let len = String.length str in
			send req.socket str 0 len [] |> ignore
		in ()
	;;
end


let handle_request bc net (req: Request.t) = 
	Printf.printf "%s\n%!" req.methodn;
	match req.methodn with
	| "getblockcount" -> 
		let bl = Int64.to_int bc.block_height in
		Request.reply req (`Int bl)
	| "getblockhash" -> (
		match req.params with
		| [`String b] -> (
			match Storage.get_blocki bc.storage @@ Int64.of_string b with
			| None -> ()
			| Some (b) -> Request.reply req (`String b.header.hash)
		)
		| _ -> ()
	)
	| "getrawblock" -> (
		match req.params with
		| [`String b] -> (
			match Storage.get_block bc.storage b with
			| None -> ()
			| Some (b) -> Request.reply req (`String (Block.serialize b))
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
		match Request.recv client_sock with
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
		bind a.socket (ADDR_INET (inet_addr_of_string "0.0.0.0", a.conf.port));
		listen a.socket 8;
    try do_listen a.socket with _ -> ()
	) else ()
;;

let shutdown a = 
	if a.conf.enable then (
    Log.fatal "Api.Rpc" "Shutdown...";
		try ( Unix.shutdown a.socket Unix.SHUTDOWN_ALL ) with | _ -> ();
		a.run <- false
  ) else ()
;;