open Log;;
open Dns;;

type t = string;;

let init () =
	Log.info "Network" "Initalization...";
 	let addrs = Dns.query "seed.bitcoin.sipa.be" in
	""
;;