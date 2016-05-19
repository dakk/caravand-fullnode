open Log;;
open Dns;;
open Params;;

type t = string;;

let init p =
	Log.info "Network" "Initalization...";
 	let addrs = Dns.query_set p.seeds in
	Log.info "Network" "Initalization done.";
	""
;;