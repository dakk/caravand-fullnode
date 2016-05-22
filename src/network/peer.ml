open Unix;;
open Log;;
open Message;;
open Params;;

type t = {
	socket	: Unix.file_descr;
	address : Unix.inet_addr;
	port	: int;
	params	: Params.t;
};;



let connect params addr port =
	Log.debug "Peer" "Connecting to peer %s:%d..." (Unix.string_of_inet_addr addr) port;
	let psock = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
	try
		Unix.connect psock (ADDR_INET (addr, port));
		Log.debug "Peer" "Connected to peer %s:%d" (Unix.string_of_inet_addr addr) port;
		Some { socket= psock; address= addr; port= port; params= params }
	with
		| _ -> 
			Log.error "Peer" "Failed to connect to peer %s:%d." (Unix.string_of_inet_addr addr) port;
			None
;;



let send peer message = 
	let data = Message.serialize peer.params message in
	Unix.send peer.socket data (Bytes.length data) 0 [] |> ignore
;;


let recv p = None;;



let handshake peer =
	let verm = {
		version		= Int32.of_int 12001;
		services	= Int64.of_int 0;
		timestamp	= Unix.gmtime (Unix.time ());
		addr_recv	= "";
		addr_from	= "";
		nonce		= Int64.of_int 0;
		user_agent	= "letchain";
		start_height= Int32.of_int 0;
		relay		= true;
	} in send peer (Message.VERSION (verm))
;;

