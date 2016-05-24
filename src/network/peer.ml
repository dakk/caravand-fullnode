open Stdint;;
open Unix;;
open Log;;
open Message;;
open Params;;
open Random;;

type t = {
	socket	: Unix.file_descr;
	address : Unix.inet_addr;
	port	: int;
	params	: Params.t;
	
	mutable last_seen	: float;
	mutable height		: int32;
	mutable user_agent	: string;
};;



let connect params addr port =
	Log.debug "Peer" "Connecting to peer %s:%d..." (Unix.string_of_inet_addr addr) port;
	let psock = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
	try
		(*Unix.set_nonblock psock;*)
		(*Make the socket a non-blocking socket, and then use select() or poll() with a timeout value to check for writability. If the select() returns with a timeout you did not connect in time, and you can close the socket and deal with the connection failure. If it returns with a completion, everything is fine and you can proceed.*)
		Unix.connect psock (ADDR_INET (addr, port));
		Log.debug "Peer" "Connected to peer %s:%d" (Unix.string_of_inet_addr addr) port;						
		Some { 
			socket= psock; 
			address= addr; 
			port= port; 
			params= params; 
			last_seen= Unix.time ();
			height= Int32.of_int 0;
			user_agent= ""; 
		}
	with
		| _ -> 
			Log.error "Peer" "Failed to connect to peer %s:%d." (Unix.string_of_inet_addr addr) port;
			None
;;



let send peer message = 
	let data = Message.serialize peer.params message in
	Unix.send peer.socket data 0 (Bytes.length data) [] |> ignore;
	Log.debug "Peer →" "%s: %s" (Unix.string_of_inet_addr peer.address) (Message.string_of_command message);
;;


let recv peer = 
	let rec recv_chunks bsize acc = 
		if bsize = (Uint32.of_int 0) then acc
		else
			let csize = if bsize > (Uint32.of_int 2048) then 2048 else Uint32.to_int bsize in
			let rdata = Bytes.create csize in
			let _ = Unix.recv peer.socket rdata 0 csize [] in
			recv_chunks (Uint32.sub bsize (Uint32.of_int csize)) (acc ^ rdata)
	in
	(* Read and parse the header*)
	let data = Bytes.create 32 in
	let _ = Unix.recv peer.socket data 0 24 [] in
	let m = Message.parse_header data in
				
	(* Read and parse the message*)
	let rdata = recv_chunks m.length "" in
	try
		let m' = Message.parse m rdata in 
		Log.debug "Peer ←" "%s: %s" (Unix.string_of_inet_addr peer.address) m.command;
		Some (m')
	with | _ ->
		Log.error "Peer ↚" "%s: %s (parse failed)" (Unix.string_of_inet_addr peer.address) m.command;
		None
;;



let handshake peer =
	let verm = {
		version		= Int32.of_int peer.params.version;
		services	= peer.params.services;
		timestamp	= Unix.time ();
		addr_recv	= { address="0000000000000000" ; services=(Int64.of_int 1) ; port= 8333 };
		addr_from	= { address="0000000000000000" ; services=(Int64.of_int 1) ; port= 8333 };
		nonce		= Random.int64 0xFFFFFFFFFFFFFFFL;
		user_agent	= "/letchain:0.12.1/";
		start_height= Int32.of_int 0;
		relay		= true;
	} in send peer (Message.VERSION (verm))
;;

