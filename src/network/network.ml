open Log;;
open Dns;;
open Params;;
open Peer;;
open Message;;

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
				| _ -> ()
			)
			; ()
		)
		| None -> ()
	);
	handle_recv n xl'
;;

let loop n = 
	let read_step = function | (rs,ws,es) -> handle_recv n rs in	
	Log.info "Network" "Starting mainloop.";
	let sockets = Hashtbl.fold (fun k v l -> (v.socket)::l  ) n.peers [] in
	
	while true do
		Unix.select sockets [] [] 1. |> read_step;
		Unix.sleep 1;
	done;
	()
;;