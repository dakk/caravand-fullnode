open Unix;;
open Yojson;;
open Yojson.Basic.Util;;


let send socket str = 
  let len = String.length str in
  send socket str 0 len [] |> ignore
;;

let recv socket = 
  let data_raw = Bytes.create 4096 in
	let reqlen = Unix.recv socket data_raw 0 4096 [] in
  String.sub data_raw 0 reqlen
;;
      
module HTTP = struct
  type met = GET | POST | DELETE | PUT;;
  type req = {
		uri		: string list;
    body	: string;
    body_json : Yojson.Basic.json option;
		rmethod	: met;
		socket 	: Unix.file_descr;
  };;
  
  let reply req status data =
    send req.socket @@ Printf.sprintf "HTTP/1.1 %d/OK\nContent-type: application/json\n\n" status;
    send req.socket @@ data;
    close req.socket
  ;;

  let reply_json req status jdata = 
    reply req status @@ Yojson.Basic.pretty_to_string jdata
  ;;

  let parse_request socket =
    let method_of_string s = match s with
    | "GET" -> GET
    | "POST" -> POST
    | "DELETE" -> DELETE
    | "PUT" -> PUT
    | _ -> GET
    in
    let data_raw = recv socket in
		let data = String.split_on_char ' ' data_raw in
		match data with
		| m :: u :: dl -> 
			let data_split = String.split_on_char '\n' data_raw in
      let body = List.nth data_split (List.length data_split - 1) in 
      let body_json = try Some (Yojson.Basic.from_string body) with _ -> None in Some ({
        uri= List.tl @@ String.split_on_char '/' u; 
        body= body;
        body_json= body_json;
        rmethod= method_of_string m;
        socket= socket
      })
    | _ -> None
  ;;
end

module JSONRPC = struct
  type req = {
    methodn : string;
    params  : Yojson.Basic.json list;
    id      : string;
		socket 	: Unix.file_descr;
  };;

  let reply req result = 
		`Assoc [
			("id", `String req.id);
			("result", result);
		] |> to_string |> send req.socket
  ;;

  let parse_request socket = 
		try (
			let data_raw = recv socket in
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
end