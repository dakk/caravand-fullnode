open Unix;;
open Yojson;;


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
  
  let reply socket status data =
    send socket @@ Printf.sprintf "HTTP/1.1 %d/OK\nContent-type: application/json\n\n" status;
    send socket @@ data;
    close socket
  ;;

  let reply_json socket status jdata = 
    reply socket status @@ Yojson.Basic.pretty_to_string jdata
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