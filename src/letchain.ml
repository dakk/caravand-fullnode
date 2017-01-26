open Network;;
open Log;;
open Params;;
open Thread;;
open Random;;
open Blockchain;;


let main () =
	let chain_job bc = 
		Blockchain.loop bc 
	in
	let net_job bc = 
		let n = Network.init bc.params in 
		Network.loop n bc
	in
	
	Random.self_init ();
	Log.info "letchain" "Starting 0.1";
	let conf = Config.load_or_init () in
 	let cn = Params.abbr_to_network conf.chain in
	Log.info "letchain" "Selected network: %s" (name_of_network cn);
	let p = Params.of_network cn in	
	
	(* Start blockchain thread *)
	let bc = Blockchain.genesis p in
	let chain_thread = Thread.create chain_job bc in
	
	(* Start network thread *)
	let net_thread = Thread.create net_job bc in
	
	Log.info "letchain" "Waiting for childs";
	Thread.join net_thread;
	Thread.join chain_thread;
	Log.info "letchain" "Exit.";
;;

main ();;
