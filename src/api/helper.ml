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
  let send_json socket status jdata = 
    send socket @@ Printf.sprintf "HTTP/1.1 %d/OK\nContent-type: application/json\n\n" status;
    send socket @@ Yojson.Basic.pretty_to_string jdata;
    close socket
  ;;
end