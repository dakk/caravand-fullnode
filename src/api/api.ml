open Log;;
open Unix;;
open Yojson.Basic.Util;;


module Request = struct
    type m = GET | POST;;

	type t = {
		uri		: string list;
		data	: string;
		rmethod	: m;
		socket 	: Unix.file_descr;
	};;

	(* 426 GET /block/tx HTTP/1.1 *)

	let recv socket = 
		let method_of_string s = match s with
		| "GET" -> GET
		| "POST" -> POST
		| _ -> GET
		in
		let data = Bytes.create 1024 in
		let reqlen = Unix.recv socket data 0 1024 [] in
		let data = String.sub data 0 reqlen in
		let data = String.split_on_char ' ' data in
		match data with
		| m :: u :: dl -> Some ({ 
			uri= String.split_on_char '/' u;
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
		send_string @@ Yojson.Basic.to_string jdata;
		close req.socket
	;;
end


let send_string sock str =
	let len = String.length str in
	send sock str 0 len [] |> ignore
;;

let handle_request bc req = 
	let rec merge_uri l = match l with
	| [] -> ""
	| x :: xl' -> x ^ "/" ^ (merge_uri xl')
	in

	Log.debug "Api" "Request: %s" @@ merge_uri req.Request.uri;	
	match req.Request.uri with
	| "block" :: bid :: _ -> 
		Printf.printf "Get block\n%!";
		Request.reply req 200 (`Assoc [("status", `String "ok")])
	| "" :: xl ->
		Printf.printf "Unhandled\n%!";
		Request.reply req 200 (`Assoc [("status", `String "ok")])
	| _ -> 
		Request.reply req 200 (`Assoc [("status", `String "err")])
;;

let loop port bc =
	let rec do_listen socket =
		let (client_sock, _) = accept socket in
		match Request.recv client_sock with
		| None -> 
			close client_sock; 
			do_listen socket
		| Some (req) ->
			handle_request bc req;
			close client_sock;
			do_listen socket
	in
	let socket = socket PF_INET SOCK_STREAM 0 in
	Log.info "Api" "Binding to port: %d" port;
	bind socket (ADDR_INET (inet_addr_of_string "0.0.0.0", port));
	listen socket 8;
	do_listen socket
;;