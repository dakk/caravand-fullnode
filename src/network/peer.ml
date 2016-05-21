open Unix;;
open Log;;

type t = {
	socket	: Unix.file_descr;
	address : Unix.inet_addr;
	port	: int;
};;


let connect addr port =
	Log.debug "Peer" "Connecting to peer %s:%d..." (Unix.string_of_inet_addr addr) port;
	let psock = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
	try
		Unix.connect psock (ADDR_INET (addr, port));
		Log.debug "Peer" "Connected to peer %s:%d" (Unix.string_of_inet_addr addr) port;
		Some { socket= psock; address= addr; port= port }
	with
		| _ -> 
			Log.error "Peer" "Failed to connect to peer %s:%d." (Unix.string_of_inet_addr addr) port;
			None
;;



let send p m = 
	()
;;


let recv p = None;;