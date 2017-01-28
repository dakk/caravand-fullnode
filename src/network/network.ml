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



let init p =
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
	let peers = init_peers p (Hashtbl.create 16) addrs 8 in
	Log.info "Network" "Connected to %d peers." (Hashtbl.length peers);
	Log.info "Network" "Initalization done.";
	{ addrs= addrs; peers= peers; params= p }
;;

let random_peer n =
	let rec rp addrs = 
		let rindex = Random.int (List.length addrs) in
		let x = List.nth addrs rindex in
		try
			let p = Hashtbl.find n.peers (Unix.string_of_inet_addr x) in
			match p.status with
			| CONNECTED -> p
			| _ -> rp addrs
		with Not_found ->
			rp addrs
	in rp n.addrs
;;


let loop n bc = 
	Log.info "Network" "Starting mainloop.";
	
	Hashtbl.iter (fun k peer -> 
		Thread.create (Peer.start peer) bc |> ignore; 
		()) n.peers;
					
	while true do
		Unix.sleep 5;
		
		(* Check for connection timeout and minimum number of peer*)		
		Hashtbl.iter (fun k peer -> 
			match peer.last_seen with
			| x when x < (Unix.time () -. 60. *. 3.) ->
				Peer.disconnect peer;
				Hashtbl.remove n.peers k;
				Log.info "Network" "Peer %s disconnected for inactivity" k;
				if Hashtbl.length n.peers < 4 then ()
				else ()
			| x when x < (Unix.time () -. 60. *. 1.) ->
				Peer.send peer (PING (Random.int64 0xFFFFFFFFFFFFFFFL))
			| _ -> () 
		) n.peers;
		
		(* Check for request *)
		Log.info "Network" "Pending request from blockchain: %d" (Cqueue.length bc.requests);

		let rec consume_requests () =
			let reqo = Cqueue.get bc.requests in	
			match reqo with
			| None -> ()
			| Some (req) ->
				(match req with
				| Blockchain.Request.REQ_HBLOCKS (h, addr)	->
					let peer = random_peer n in
					let msg = {
						version= Int32.of_int 1;
						hashes= h;
						stop= Hash.zero ();
					} in Peer.send peer (Message.GETHEADERS msg)
				| Blockchain.Request.REQ_BLOCKS (hs, addr)	->
					(* Qua andra' un GETDATA *)
					let peer = random_peer n in
					let rec create_invs hs acc = match hs with
					| [] -> acc
					| h::hs' -> create_invs hs' ((INV_BLOCK (h)) :: acc)
					in Peer.send peer (Message.GETDATA (create_invs hs []));
				| _ -> ());
				consume_requests ()
		in consume_requests ();
	done;
	()
;;
