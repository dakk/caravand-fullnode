open Unix;;
open Utils;;


let query server =
	try
		Log.debug "Dns" "Fetching peer list from seed %s..." server;
		let host = Unix.gethostbyname server in
		let addrs = Array.to_list host.h_addr_list in
		Log.debug "Dns" "Fetched %d peer addresses from seed %s" (List.length addrs) server;
		addrs
	with 
		Not_found -> 
			Log.error "Dns" "No peer found from seed %s" server; []
;;

let query_set2 servers = [inet_addr_of_string "127.0.0.1"];;

let query_set servers =
	let rec qs sl = match sl with
	| s::sl' -> (query s)@(qs sl')
	| [] -> []
	in
	let addrs = qs servers in
	Log.info "Dns" "Fetched %d peers from the seed set" (List.length addrs);
	addrs
;;
