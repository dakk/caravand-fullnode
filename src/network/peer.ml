open Stdint;;
open Unix;;
open Log;;
open Message;;
open Params;;
open Blockchain;;
open Random;;

type status = 
	| CONNECTED
	| DISCONNECTED
	| WAITPING of int64
;;

type t = {
	socket		: Unix.file_descr;
	address 	: Unix.inet_addr;
	port		: int;
	params		: Params.t;
	
	mutable received	: int;
	mutable sent		: int;

	mutable status		: status;
	mutable last_seen	: float;
	mutable height		: int32;
	mutable user_agent	: string;
};;


let byten_to_string b =
	match b with
	| b' when b < 1024 -> Printf.sprintf "%dB" b'
	| b' when b < 1024 * 1024 -> Printf.sprintf "%dKB" (b' / 1024)
	| b' -> Printf.sprintf "%dMB" (b' / 1024 / 1024)
;;

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

	received	= 0;
	sent		= 0;

	last_seen	= Unix.time ();
	height		= Int32.of_int 0;
	user_agent	= ""; 
	status		= DISCONNECTED;
};;

let connect peer =
	Log.debug "Peer" "Connecting to peer %s:%d..." (Unix.string_of_inet_addr peer.address) peer.port;
	try
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
	try (
		let wl = Unix.single_write peer.socket data 0 (Bytes.length data) in
		Log.debug "Peer →" "%s: %s (s: %s, r: %s)" (Unix.string_of_inet_addr peer.address) 
				(string_of_command message) (byten_to_string peer.sent) (byten_to_string peer.received);
		peer.sent <- peer.sent + wl;
	) with
	| _ -> ()
;;


let recv peer = 
	let rec recv_chunks bsize acc = 
		if bsize = Uint32.zero then 
			let res = Buffer.to_bytes acc in
			Buffer.clear acc; Some (res)
		else
			let csize = if bsize >= (Uint32.of_int 0xFFF) then 0xFFF else Uint32.to_int bsize in
			let rdata = Bytes.create csize in
			let rl = Unix.read peer.socket rdata 0 csize in
			match rl with
			| rl when rl < 0 -> None
			| rl when rl = 0 ->	recv_chunks bsize acc
			| rl when rl > 0 ->
				Buffer.add_bytes acc (Bytes.sub_string rdata 0 rl);
				recv_chunks (Uint32.sub bsize (Uint32.of_int rl)) acc
	in
	(* Read and parse the header*)
	let data = Bytes.create 24 in

	try (
		match Unix.recv peer.socket data 0 24 [] with
		| rl when rl < 0 -> peer.status <- DISCONNECTED; None
		| rl when rl = 0 ->	None
		| rl when rl > 0 -> (
			let m = Message.parse_header data in
						
			(* Read and parse the message*)
			peer.received <- peer.received + (Uint32.to_int m.length);
			match recv_chunks m.length (Buffer.create 4096) with
			| None -> peer.status <- DISCONNECTED; None
			| Some (rdata) -> (
				let m' = Message.parse m rdata in 

				Log.debug "Peer ←" "%s: %s (s: %s, r: %s)" (Unix.string_of_inet_addr peer.address) 
					m.command (byten_to_string peer.sent) (byten_to_string peer.received);
				
				Some (m')
			)
		)
	) with 
	| _ -> 
		peer.status <- DISCONNECTED; 
		Log.error "Peer ↚" "Invalid message from %s" (Unix.string_of_inet_addr peer.address);
		None
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
						(*Log.info "Network" "Got inv tx %s" txid;*)
						Cqueue.add bc.resources (Blockchain.Resource.RES_INV_TXS ([txid], peer.address));
					| INV_BLOCK (bhash) -> 
						(*Log.info "Network" "Got inv block %s" bhash;*)
						Cqueue.add bc.resources (Blockchain.Resource.RES_INV_BLOCKS ([bhash], peer.address));
					| _ -> ()
				) in vis xl  
			| [] -> ()
			in vis i;
		| BLOCK (b) -> 
			Cqueue.add bc.resources (Blockchain.Resource.RES_BLOCK (b));			
		| HEADERS (hl) ->
			Cqueue.add bc.resources (Blockchain.Resource.RES_HBLOCKS (hl));
		| _ -> ()
	)
;;

let start peer bc = 
	let read_step = function | (rs,ws,es) -> handle peer bc in	
	
	match connect peer with 
	| DISCONNECTED -> ()
	| _ -> (
		handshake peer;
		
		while peer.status <> DISCONNECTED do
			Unix.select [peer.socket] [] [] 5.0 |> read_step; ();
		done
	)
;;