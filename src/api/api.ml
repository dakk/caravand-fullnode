open Log;;
open Unix;;
open Blockchain;;
open Block;;
open Block.Header;;
open Params;;
open Stdint;;
open Yojson.Basic.Util;;
open Yojson.Basic;;


module Request = struct
    type m = GET | POST | PUT | DELETE;;

	type t = {
		uri		: string list;
		data	: string;
		rmethod	: m;
		socket 	: Unix.file_descr;
	};;

	let recv socket = 
		let method_of_string s = match s with
		| "GET" -> GET
		| "POST" -> POST
		| "DELETE" -> DELETE
		| "PUT" -> PUT
		| _ -> GET
		in
		let data = Bytes.create 1024 in
		let reqlen = Unix.recv socket data 0 1024 [] in
		let data = String.split_on_char ' ' @@ String.sub data 0 reqlen in
		match data with
		| m :: u :: dl -> Some ({ 
			uri= List.tl @@ String.split_on_char '/' u; 
			data= "";
			rmethod= method_of_string m;
			socket= socket
		})
		| _ -> None 
	;;

	let reply req status jdata =
		let send_string str = 
			let len = String.length str in
			send req.socket str 0 len [] |> ignore
		in
		send_string @@ Printf.sprintf "HTTP/1.1 %d/OK\nContent-type: application/json\n\n" status;
		send_string @@ Yojson.Basic.pretty_to_string jdata;
		close req.socket
	;;
end


let send_string sock str =
	let len = String.length str in
	send sock str 0 len [] |> ignore
;;

let handle_request bc req = 
	let not_found () = Request.reply req 404 (`Assoc [("status", `String "err")]) in

	Log.debug "Api ↔" "%s" @@ List.fold_left (fun x acc -> x ^ "/" ^ acc) "" req.Request.uri;	
	match req.Request.rmethod, req.Request.uri with

	(* Push a tx *)
	| (Request.POST, "tx" :: []) ->
		Request.reply req 200 (`Assoc [
			("status", `String "ok");
			("txid", `String "asdasd")
		])

	(* Get address info *)
	| (Request.GET, "address" :: addr :: []) -> 
		let ad = Storage.get_address bc.storage addr in
		Request.reply req 200 (`Assoc [
			("status", `String "ok");
			("address", `Assoc [
				("address", `String addr);
				("balance", `String (Uint64.to_string ad.balance));
				("unconfirmed_balance", `String (Uint64.to_string @@ Uint64.zero));
				("sent", `String (Uint64.to_string ad.sent));
				("received", `String (Uint64.to_string ad.received));
				("txs", `String (Uint64.to_string ad.txs))				
			])
		])

	(* Get address txs *)
	| (Request.GET, "address" :: addr :: "txs" :: []) -> 
		Request.reply req 200 (`Assoc [
			("status", `String "ok");
			("txs", `Assoc [
				
			])
		])

	(* Get address utxo *)
	| (Request.GET, "address" :: addr :: "utxo" :: []) -> 
		Request.reply req 200 (`Assoc [
			("status", `String "ok");
			("utxo", `Assoc [
				
			])
		])

	(* Get tx info *)
	| (Request.GET, "tx" :: txid :: []) -> 
		Request.reply req 200 (`Assoc [
			("status", `String "ok");
			("tx", `Assoc [
				("txid", `String txid)
			])
		])

	(* Get block info by index *)
	| (Request.GET, "block" :: "i" :: bid :: []) -> 
		Request.reply req 200 (`Assoc [
			("status", `String "ok");
			("block", `Assoc [
				("hash", `String bid)
			])
		])

	(* Get block info by hash *)
	| (Request.GET, "block" :: bid :: []) -> 
		Request.reply req 200 (`Assoc [
			("status", `String "ok");
			("block", `Assoc [
				("hash", `String bid)
			])
		])

	(* Get chain state *)
	| (Request.GET, "" :: []) ->
		Request.reply req 200 (`Assoc [
			("status", `String "ok");
			("state", `Assoc [
				("chain", `String (Params.name_of_network bc.params.network));
				("sync", `Bool bc.sync);
				("height", `Int (Int64.to_int bc.block_height));
				("last", `String bc.block_last.header.hash);
				("time", `String (Timediff.diffstring (Unix.time ()) bc.block_last.header.time));
				("header", `Assoc [
					("sync", `Bool bc.sync_headers);
					("height", `Int (Int64.to_int bc.header_height));
					("last", `String (bc.header_last.hash));
					("time", `String (Timediff.diffstring (Unix.time ()) bc.header_last.time));
				]);
				("mempool", `Assoc [
					("size", `Int 0);
					("n", `Int 0);
					("fees", `Int 0)
				]);
				("txs", `Int (Uint64.to_int bc.storage.chainstate.txs));
				("utxos", `Int (Uint64.to_int bc.storage.chainstate.utxos))
			])
		])

	(* Not found *)
	| (_, _) -> 
		not_found ()
;;

let loop port bc =
	let rec do_listen socket =
		let (client_sock, _) = accept socket in
		match Request.recv client_sock with
		| None -> 
			(*close client_sock;*)
			do_listen socket
		| Some (req) ->
			handle_request bc req;
			(*close client_sock;*)
			do_listen socket
	in
	let socket = socket PF_INET SOCK_STREAM 0 in
	Log.info "Api" "Binding to port: %d" port;
	bind socket (ADDR_INET (inet_addr_of_string "0.0.0.0", port));
	listen socket 8;
	do_listen socket
;;