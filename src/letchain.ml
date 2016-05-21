open Network;;
open Log;;
open Params;;
open Thread;;


let main () =
	let chain_job p = 
		() 
	in
	let net_job p = 
		let n = Network.init p in 
		Network.loop n
	in
	
	Log.info "letchain" "Starting 0.1";
	let cn = Params.BTC in
	Log.info "letchain" "Selected network: %s" (name_of_network cn);
	let p = Params.of_network cn in	
	
	(* Start blockchain thread *)
	(* Start network thread *)
	let net_thread = Thread.create net_job p in
	let chain_thread = Thread.create chain_job p in
	
	Log.info "letchain" "Waiting for childs";
	Thread.join net_thread;
	Log.info "letchain" "Exit.";
;;

main ();;
