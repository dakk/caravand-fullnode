open Log;;
open Dns;;
open Params;;
open Peer;;

type t = {
	addrs:  Unix.inet_addr list;
	peers:	(string, Peer.t) Hashtbl.t;
	params: Params.t;
};;


let peer_of_socket fd peers =
	try
		begin
			match Unix.getpeername fd with
			| Unix.ADDR_INET (a, p) -> 
				(try Some (Hashtbl.find peers (Unix.string_of_inet_addr a))
				with | Not_found -> None)
			| _ -> None
		end
	with | _ -> None
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
					(*Peer.handshake peer;*)
					Hashtbl.add pt a' peer; 
					connect par pt al' (n-1)
				| None -> connect par pt al' n
;;


let init p =
	Log.info "Network" "Initalization...";
 	let addrs = Dns.query_set p.seeds in
	let peers = connect p (Hashtbl.create 16) addrs 6 in
	Log.info "Network" "Connected to %d peers." (Hashtbl.length peers);
	Log.info "Network" "Initalization done.";
	{ addrs= addrs; peers= peers; params= p }
;;


let loop n = 
	let read_step sockets = 
		let rec read' sockets =
			match sockets with
			| [] -> ()
			| x::xl' ->
				match peer_of_socket x n.peers with
				| Some (peer) ->					
					let data = Bytes.create 32 in
					let rb = Unix.recv peer.socket data 0 24 [] in
					let m = Message.parse_header data in
					Log.info "Network" "Receive message from %s: %s" (Unix.string_of_inet_addr peer.address) m.command
						
					(* Parse the header *)
					(* Read the other data *)
					(* Parse the message *)
					(* Send info to the blockchain module *)		
					; ()
				| None -> ()
				; 
				read' xl'
		in
		match sockets with | (rs,ws,es) ->
			Log.info "Network" "Sockets: %d %d %d" (List.length rs) (List.length ws) (List.length es);
			read' rs
	in	
	Log.info "Network" "Starting mainloop.";
	let sockets = Hashtbl.fold (fun k v l -> (v.socket)::l  ) n.peers [] in
	
	while true do
		Unix.select sockets [] [] 1. |> read_step;
		Unix.sleep 1;
	done;
	()
;;