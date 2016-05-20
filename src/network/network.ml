open Log;;
open Dns;;
open Params;;
open Peer;;
open Async;;

type t = {
	addrs:  Unix.inet_addr list;
	peers:	(string, Peer.t) Hashtbl.t;
	params: Params.t;
};;


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
			let p = Peer.connect a par.port in
			match p with
				| Some (peer) -> 
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


let loop n = 
	Log.info "Network" "Starting mainloop.";
	()
;;