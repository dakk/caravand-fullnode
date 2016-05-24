open Block;;
open Log;;
open Dns;;
open Params;;
open Peer;;
open Message;;
open Blockchain;;

type t = {
	addrs:  Unix.inet_addr list;
	peers:	(string, Peer.t) Hashtbl.t;
	params: Params.t;
};;


let peer_of_socket fd peers =
	try (
		match Unix.getpeername fd with
		| Unix.ADDR_INET (a, p) -> 
			(try Some (Hashtbl.find peers (Unix.string_of_inet_addr a))
			with | Not_found -> None)
		| _ -> None
	) with | _ -> None
;;

let rec connect par pt addrs n =
	match (n, addrs) with
	| (0, a::al') -> pt
	| (0, []) -> pt
	| (n', []) -> pt
	| (n', a::al') ->  
		let a' = Unix.string_of_inet_addr a in
		try
			let _ = Hashtbl.find pt a' in
			connect par pt al' n 
		with Not_found -> 
			let p = Peer.connect par a par.port in
			match p with
				| Some (peer) -> 
					Peer.handshake peer;
					
					(* TEST: getheaders *) 
					Peer.send peer (GETHEADERS {
						version= Int32.of_int 70001;
						count=1;
						hashes= ["\x00\x00\x00\x00\x00\x19\xd6\x68\x9c\x08\x5a\xe1\x65\x83\x1e\x93\x4f\xf7\x63\xae\x46\xa2\xa6\xc1\x72\xb3\xf1\xb6\x0a\x8c\xe2\x6f"];
						stop= String.make 32 '\x00';
					});
					
					Hashtbl.add pt a' peer; 
					connect par pt al' (n-1)
				| None -> connect par pt al' n
;;


let init p =
	Log.info "Network" "Initalization...";
 	let addrs = Dns.query_set p.seeds in
	let peers = connect p (Hashtbl.create 16) addrs 2 in
	Log.info "Network" "Connected to %d peers." (Hashtbl.length peers);
	Log.info "Network" "Initalization done.";
	{ addrs= addrs; peers= peers; params= p }
;;


let rec handle_recv n = function
	| [] -> ()
	| x::xl' -> (
		match peer_of_socket x n.peers with
		| Some (peer) -> (					
			let m = Peer.recv peer in
			match m with
			| None -> ()
			| Some (m') -> (
				peer.last_seen <- Unix.time ();
				match m' with 
				| PING (p) -> Peer.send peer (PONG (p));
				| VERSION (v) ->
					peer.height <- v.start_height;
					peer.user_agent <- v.user_agent;
					Peer.send peer VERACK;
					Log.info "Network" "Peer %s with agent %s starting from height %d" 
						(Unix.string_of_inet_addr peer.address) (peer.user_agent) (Int32.to_int peer.height);
				| INV (i) ->
					let rec vis h = match h with
					| x::xl ->
						let _ = (match x with
						| INV_TX (txid) ->
							Log.info "Network" "Got inv tx %s" (Hash.to_string txid);
						) in vis xl  
					| [] -> ()
					in vis i;
					Log.info "Network" "Received %d inv" (List.length i);
					
				| HEADERS (hl) ->
					let rec vis h = match h with
					| x::xl ->
						Log.info "Network" "Got header %s" (Hash.bhash_to_string x.Block.Header.prev_block);
						vis xl  
					| [] -> ()
					in vis hl
				| _ -> ()
			)
			; ()
		)
		| None -> ()
	);
	handle_recv n xl'
;;

let loop n bc = 
	let read_step = function | (rs,ws,es) -> handle_recv n rs in	
	Log.info "Network" "Starting mainloop.";
	let sockets = Hashtbl.fold (fun k v l -> (v.socket)::l) n.peers [] in
	
	while true do
		(* Read new data *)
		Unix.select sockets [] [] 5.0 |> read_step;

		(* Check for connection timeout and minimum number of peer*)		
		Hashtbl.iter (fun k peer -> 
			if peer.last_seen < (Unix.time () -. 60. *. 5.) then (
				Hashtbl.remove n.peers k;
				Log.info "Network" "Peer %s disconnected for inactivity" k;
				if Hashtbl.length n.peers < 4 then
					(* Connect to new peers *) 
					()
				else
					()
			) else (			
				if peer.last_seen < (Unix.time () -. 60. *. 3.) then
					Peer.send peer (PING (Random.int64 0xFFFFFFFFFFFFFFFL))
				else ()
			) 
		) n.peers;
		
		(* Check for request *)
		(*Log.info "Network" "Pending request from blockchain: %d" (Queue.length bc.queue_req);*)
	done;
	()
;;