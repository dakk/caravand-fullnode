open Unix;;
open Log;;

let query ~server =
	let host = Unix.gethostbyname server in
	Log.info "Dns" "Fetching peer list..."; 
	let addrs = Array.to_list host.h_addr_list in
	(*Log.info "Dns" "Fetched %d peer addresses" (List.length addrs);*)
	addrs
;;
