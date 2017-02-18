open Block;;
open Log;;
open Dns;;
open Params;;
open Peer;;
open Message;;
open Blockchain;;

type t = {
	addrs: 		Unix.inet_addr list;
	peers:		(string, Peer.t) Hashtbl.t;
	params: 	Params.t;
	peers_n:	int;
};;


let init p pc =
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
				let peer = Peer.create par a par.port in
				Hashtbl.add pt a' peer;
				init_peers par pt al' (n-1)
	in
	Log.info "Network" "Initalization...";
 	let addrs = Dns.query_set p.seeds in
	let peers = init_peers p (Hashtbl.create pc) addrs pc in
	Log.info "Network" "Connected to %d peers." (Hashtbl.length peers);
	Log.info "Network" "Initalization done.";
	{ addrs= addrs; peers= peers; params= p; peers_n= pc }
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




let loop n bc = 
	Log.info "Network" "Starting mainloop.";
	
	Hashtbl.iter (fun k peer -> 
		Thread.create (Peer.start peer) bc |> ignore; 
		()
	) n.peers;
					
	while true do
		Unix.sleep 2;

		(* Check for connection timeout and minimum number of peer*)		
		Hashtbl.iter (fun k peer -> 
			match (peer.status, peer.last_seen) with
			| (WAITPING (rnd), x) when x < (Unix.time () -. 60. *. 2.0) ->
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
			Hashtbl.fold (fun k p c -> (match p.status with | DISCONNECTED -> c | _ -> c + 1)) n.peers 0
		in 
			(*Log.info "Network" "Connected peers: %d" connected_peers;*)
			match connected_peers with
			| cp when cp < n.peers_n ->
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
							let peer = Peer.create n.params a n.params.port in
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
				| Blockchain.Request.REQ_HBLOCKS (h, addr)	->
					let msg = {
						version= Int32.of_int 1;
						hashes= h;
						stop= Hash.zero ();
					} in send n (Message.GETHEADERS msg)
				| Blockchain.Request.REQ_BLOCKS (hs, addr)	->
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
