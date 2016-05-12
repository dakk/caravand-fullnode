open Core.Std;;
open Printf;;

let query ~server =
	let addr = Unix.Inet_addr.of_string_or_getbyname server in
	let st = Unix.Inet_addr.to_string addr in
	Printf.printf "%s" st 
;;
