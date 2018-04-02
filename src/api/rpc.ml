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
    jsonrpc : string;
    methodn : string;
    params  : Yojson.Basic.json list;
    id      : string;
		socket 	: Unix.file_descr;
	};;

	let recv socket = 
		let data_raw = Bytes.create 4096 in
		let reqlen = Unix.recv socket data_raw 0 4096 [] in
		let data_raw = String.sub data_raw 0 reqlen in
    let data = try Some (Yojson.Basic.from_string data_raw) with _ -> None in
    match data with 
    | Some (j) -> Some ({
      socket= socket;
      id= j |> member "id" |> to_string;
      params= j |> member "params" |> to_list;
      methodn= j |> member "method" |> to_string;
      jsonrpc= j |> member "jsonrpc" |> to_string;
    })
    | None -> None
	;;

	let reply req status jdata =
		let send_string str = 
			let len = String.length str in
			send req.socket str 0 len [] |> ignore
		in ()
	;;
end


let handle_request bc net req = 
  ()
;;

type t = {
	blockchain : Chain.t;
  network 	 : Net.t;
  conf       : Config.rpc;
	mutable run: bool;
};;

let init (rconf: Config.rpc) bc net = { blockchain= bc; conf= rconf; network= net; run = rconf.enable };;

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
		let socket = socket PF_INET SOCK_STREAM 0 in
		Log.info "Api.Rpc" "Binding to port: %d" a.conf.port;
		bind socket (ADDR_INET (inet_addr_of_string "0.0.0.0", a.conf.port));
		listen socket 8;
    try do_listen socket with _ -> ()
	) else ()
;;

let shutdown a = 
	if a.conf.enable then (
    Log.fatal "Api.Rpc" "Shutdown...";
    a.run <- false;
  ) else ()
;;