open Bitcoinml;;
open Utils;;
open Conv;;
open Dns;;
open Peer;;
open Message;;
open Blockchain;;
open Params;;

type t = {
	addrs: 		Unix.inet_addr list;
	peers:		(string, Peer.t) Hashtbl.t;
	params: 	Params.t;
	config:		Config.t;
};;


let init p conf =
	let rec init_peers par pt addrs n =
		match (n, addrs) with
		| (0, a::al') -> pt
		| (0, []) -> pt
		| (n', []) -> pt
		| (n', a::al') ->  
			let a' = Unix.string_of_inet_addr a in
			try
				let _ = Hashtbl.find pt a' in
				init_peers par pt al' n 
			with Not_found -> 
				let peer = Peer.create par conf a par.port in
				Hashtbl.add pt a' peer;
				init_peers par pt al' (n-1)
	in
	Log.info "Network" "Initalization...";
 	let addrs = Dns.query_set p.seeds in
	let peers = init_peers p (Hashtbl.create conf.peers) addrs conf.peers in
	Log.info "Network" "Connected to %d peers." (Hashtbl.length peers);
	Log.info "Network" "Initalization done.";
	{ addrs= addrs; peers= peers; params= p; config= conf  }
;;

let send n m =
	let rec best_peers addrs bests = match addrs with
	| [] -> bests
	| a::addrs' ->
		match Hashtbl.mem n.peers (Unix.string_of_inet_addr a) with
		| false -> best_peers addrs' bests
		| true ->
			let p = Hashtbl.find n.peers (Unix.string_of_inet_addr a) in
			match p.status, p.last_seen with
			| CONNECTED, ls when (Unix.time () -. 30.) < ls -> best_peers addrs' (p::bests)
			| _, _ -> best_peers addrs' bests
	in
	let bests = best_peers n.addrs [] in
	let peer = match List.length bests with
	| 0 -> None
	| _ -> Some (List.nth bests (Random.int (List.length bests)))
	in 
	match peer with 
	| Some (peer) -> Peer.send peer m; ()
	| None -> ()
;;


let peer_of_addr n addr =
	try
		Some (Hashtbl.find n.peers @@ Unix.string_of_inet_addr addr)
	with
	| _ -> None
;;
	


let loop n bc = 
	Log.info "Network" "Starting mainloop.";
	
	Hashtbl.iter (fun k peer -> 
		Thread.create (Peer.start peer) bc |> ignore; 
		()
	) n.peers;
					
	while true do
		Unix.sleep 2;

		(* Print network stats *)
		let connected_peers = Hashtbl.fold (fun k p c -> (match p.status with | DISCONNECTED -> c | _ -> c + 1)) n.peers 0 in
		let waitping_peers = Hashtbl.fold (fun k p c -> (match p.status with | WAITPING (x) -> c + 1 | _ -> c)) n.peers 0 in
		let sent = Hashtbl.fold (fun k p c -> p.sent + c) n.peers 0 in 
		let received = Hashtbl.fold (fun k p c -> p.received + c) n.peers 0 in 
		Log.info "Network" "Stats: %s sent, %s received, %d connected peers (%d waitping)" (byten_to_string sent) (byten_to_string received) connected_peers waitping_peers;

		(* Check for connection timeout and minimum number of peer*)		
		Hashtbl.iter (fun k peer -> 
			match (peer.status, peer.last_seen) with
			| (WAITPING (rnd), x) when x < (Unix.time () -. 60. *. 1.5) ->
				Peer.disconnect peer;
				Log.info "Network" "Peer %s disconnected for inactivity" k;
				if Hashtbl.length n.peers < 4 then ()
				else ()
			| (CONNECTED, x) when x < (Unix.time () -. 60. *. 1.0) ->
				let rnd = Random.int64 0xFFFFFFFFFFFFFFFL in
				peer.status <- WAITPING (rnd);
				Peer.send peer (PING (rnd))
			| (DISCONNECTED, x) ->
				Hashtbl.remove n.peers k
			| _ -> () 
		) n.peers;

		(* Count available peers and reconnect to others *)		
		let connected_peers = 
			int_of_float (Hashtbl.fold (fun k p c -> (match p.status with | DISCONNECTED -> c | WAITPING (x) -> c +. 0.5 | _ -> c +. 1.0)) n.peers 0.0)
		in 
			(*Log.info "Network" "Connected peers: %d" connected_peers;*)
			match connected_peers with
			| cp when cp < n.config.peers ->
				let rec iterate_connect addrs = 
					match List.length addrs with
					| 0 -> 0
					| _ ->
						let rindex = Random.int (List.length addrs) in
						let a = List.nth addrs rindex in	
						try 
							Hashtbl.find n.peers (Unix.string_of_inet_addr a) |> ignore;
							iterate_connect addrs
						with
						| Not_found ->
							let peer = Peer.create n.params n.config a n.params.port in
							Hashtbl.add n.peers (Unix.string_of_inet_addr a) peer;
							Thread.create (Peer.start peer) bc |> ignore; 
							1
						| _ -> 0
				in
				Log.error "Network" "Peers below the number of peers";
				let nc = iterate_connect n.addrs in
				Log.info "Network" "Connected to %d new peers" nc;
				()
			| _ -> ()
		;
		
		(* Check for request *)
		(*Log.info "Network" "Pending request from blockchain: %d" (Cqueue.length bc.requests);*)
		let rec consume_requests () =
			let reqo = Cqueue.get bc.requests in	
			match reqo with
			| None -> ()
			| Some (req) ->
				(match req with
				| Chain.Request.RES_HBLOCKS (hl, addr) -> (
					match peer_of_addr n addr with
					| None -> ()
					| Some (p) -> Peer.send p @@ Message.HEADERS (hl))
				| Chain.Request.REQ_HBLOCKS (h, addr)	->
					let msg = {
						version= Int32.of_int 1;
						hashes= h;
						stop= Hash.zero;
					} in send n @@ Message.GETHEADERS (msg)
				| Chain.Request.REQ_BLOCKS (hs, addr)	->
					let rec create_invs hs acc = match hs with
					| [] -> acc
					| h::hs' -> create_invs hs' ((INV_BLOCK (h)) :: acc)
					in send n (Message.GETDATA (create_invs hs []));
				| _ -> ());
				consume_requests ()
		in 
		let available_peers = 
			Hashtbl.fold (fun k p c -> (match p.status with | CONNECTED -> c + 1 | _ -> c)) n.peers 0
		in match available_peers with 
		| 0 -> ()
		| n -> consume_requests ();
	done;
	()
;;
