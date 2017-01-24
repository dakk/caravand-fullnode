open Stdint;;
open Unix;;
open Log;;
open Message;;
open Params;;
open Blockchain;;
open Random;;

type status = 
	  CONNECTED
	| DISCONNECTED
	| WAITPING of int64
;;

type t = {
	socket	: Unix.file_descr;
	address : Unix.inet_addr;
	port	: int;
	params	: Params.t;
	
	mutable status		: status;
	mutable last_seen	: float;
	mutable height		: int32;
	mutable user_agent	: string;
};;


let rec is_readable s = 
	match String.length s with
	| 0 -> true
	| n -> 
		let c = Char.code (String.get s 0) in
		if c >= Char.code 'a' && c <= Char.code 'z' then
			is_readable (String.sub s 1 (n - 1))
		else
			false  
;;


let create params addr port = {
	socket		= Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0; 
	address		= addr; 
	port		= port; 
	params		= params; 
	last_seen	= Unix.time ();
	height		= Int32.of_int 0;
	user_agent	= ""; 
	status		= DISCONNECTED;
};;

let connect peer =
	Log.debug "Peer" "Connecting to peer %s:%d..." (Unix.string_of_inet_addr peer.address) peer.port;
	try
		(*Unix.set_nonblock psock;*)
		(*Make the socket a non-blocking socket, and then use select() or poll() with a timeout value to check for writability. If the select() returns with a timeout you did not connect in time, and you can close the socket and deal with the connection failure. If it returns with a completion, everything is fine and you can proceed.*)
		Unix.connect peer.socket (ADDR_INET (peer.address, peer.port));
		Log.debug "Peer" "Connected to peer %s:%d" (Unix.string_of_inet_addr peer.address) peer.port;
		peer.status <- CONNECTED; CONNECTED					
	with
		| _ -> 
			peer.status <- DISCONNECTED; 
			Log.error "Peer" "Failed to connect to peer %s:%d." (Unix.string_of_inet_addr peer.address) peer.port;
			DISCONNECTED
;;



let send peer message = 
	let data = Message.serialize peer.params message in
	Unix.send peer.socket data 0 (Bytes.length data) [] |> ignore;
	Log.debug "Peer →" "%s: %s" (Unix.string_of_inet_addr peer.address) (Message.string_of_command message);
;;


let recv peer = 
	let rec burn_chunks bsize =
		if bsize = Uint32.zero then ()
		else
			let csize = if bsize >= (Uint32.of_int 0xFFFFFF) then 0xFFFFFF else Uint32.to_int bsize in
			let rdata = Bytes.create csize in
			let _ = Unix.recv peer.socket rdata 0 csize [] in
			burn_chunks (Uint32.sub bsize (Uint32.of_int csize)) 
	in
	let rec recv_chunks bsize acc = 
		if bsize = Uint32.zero then Bytes.concat "" acc
		else
			let csize = if bsize >= (Uint32.of_int 0xFFFFFF) then 0xFFFFFF else Uint32.to_int bsize in
			let rdata = Bytes.create csize in
			let _ = Unix.recv peer.socket rdata 0 csize [] in
			recv_chunks (Uint32.sub bsize (Uint32.of_int csize)) (acc @ [rdata])
	in
	(* Read and parse the header*)
	let data = Bytes.create 24 in
	let _ = Unix.recv peer.socket data 0 24 [] in
	let m = Message.parse_header data in
				
	(* Read and parse the message*)
	if m.length > (Uint32.of_string "9999999") then (
		burn_chunks m.length; 
		Log.error "Peer ↚" "%s: %s (skipped big message)" (Unix.string_of_inet_addr peer.address) m.command;
		None
	) else (
		let rdata = recv_chunks m.length [] in
		try
			let m' = Message.parse m rdata in 
			Log.debug "Peer ←" "%s: %s" (Unix.string_of_inet_addr peer.address) m.command;
			Some (m')
		with | _ ->
			if is_readable m.command then
				Log.error "Peer ↚" "%s: %s (parse failed)" (Unix.string_of_inet_addr peer.address) m.command
			else
				Log.error "Peer ↚" "%s: not readable (parse failed)" (Unix.string_of_inet_addr peer.address);
			None
	)
;;



let handshake peer =
	let verm = {
		version		= Int32.of_int peer.params.version;
		services	= peer.params.services;
		time		= Unix.time ();
		addr_recv	= { address="0000000000000000" ; services=(Int64.of_int 1) ; port= 8333 };
		addr_from	= { address="0000000000000000" ; services=(Int64.of_int 1) ; port= 8333 };
		nonce		= Random.int64 0xFFFFFFFFFFFFFFFL;
		user_agent	= "/letchain:0.12.1/";
		start_height= Int32.of_int 0;
		relay		= true;
	} in send peer (Message.VERSION (verm))
;;



let disconnect peer = Unix.shutdown peer.socket Unix.SHUTDOWN_ALL;;

let handle peer bc = 
	let m = recv peer in
	match m with
	| None -> ()
	| Some (m') -> (
		peer.last_seen <- Unix.time ();
		match m' with 
		| PING (p) -> send peer (PONG (p));
		| VERSION (v) ->
			peer.height <- v.start_height;
			peer.user_agent <- v.user_agent;
			send peer VERACK;
			Log.info "Network" "Peer %s with agent %s starting from height %d" 
				(Unix.string_of_inet_addr peer.address) (peer.user_agent) (Int32.to_int peer.height);
		| INV (i) ->
			let rec vis h = match h with
			| x::xl ->
				let _ = (match x with
					| INV_TX (txid) -> 
						Log.info "Network" "Got inv tx %s" txid;
						Blockchain.add_resource bc (Blockchain.Resource.RES_INV_TXS ([txid], peer.address));
					| INV_BLOCK (bhash) -> 
						Log.info "Network" "Got inv block %s" bhash;
						Blockchain.add_resource bc (Blockchain.Resource.RES_INV_BLOCKS ([bhash], peer.address));
					| _ -> ()
				) in vis xl  
			| [] -> ()
			in vis i;
			(*send peer (GETDATA (i));*)			
		| HEADERS (hl) ->
			let rec vis h = match h with
				| x::xl ->
					Log.info "Network" "Got block header %s" x.Block.Header.hash;
					vis xl  
				| [] -> ()
			in vis hl;
			Blockchain.add_resource bc (Blockchain.Resource.RES_HBLOCKS (hl));
		| _ -> ()
	)
;;

let start peer bc = 
	let read_step = function | (rs,ws,es) -> handle peer bc in	
	
	match connect peer with 
	| DISCONNECTED -> ()
	| _ -> 
		handshake peer;
		(*send peer (GETHEADERS ({ version= Int32.one; hashes= [bc.header_last]; stop= Hash.zero () }));*)
		
		while peer.status <> DISCONNECTED do
			Unix.select [peer.socket] [] [] 5.0 |> read_step; ();
			(* send peer (GETHEADERS ({ version= Int32.one; hashes= [bc.header_last]; stop= Hash.zero () }))*)
			(* This should get requests filtered by addr Blockchain.get_request*)
		done
;;