open Thread;;
open Blockchain;;
open Network;;
open Utils;;
open Unix;;

type t = {
  chain       : Chain.t;
  net         : Net.t;
  chain_thread: Thread.t;
  net_thread  : Thread.t;
};;

let init ?directory:(directory="") ?network:(network="XBT") ?peers:(peers=6) ?loglevel:(loglevel=0) mode = 
	let directory = if directory = "" then (Unix.getenv "HOME") ^ "/.letchain/" else directory in

	let chain_job bc = Chain.loop bc in
	let net_job (bc, net, conf) = Net.loop net bc in
	
	Log.set_level loglevel;
	Random.self_init ();
	Log.info "letchain" "Starting 0.1";
	let conf = { (directory |> Config.load_or_init) with peers=peers; log_level=loglevel; mode=mode } in
 	let cn = Params.abbr_to_network conf.chain in
	if cn = NOTFOUND then
		Log.info "letchain" "Invalid chain"
	else 	
		Log.info "letchain" "Selected network: %s" (Params.name_of_network cn);
		let p = Params.of_network cn in	
		
		let bc = Chain.load conf.path conf p in
		let n = Net.init bc.Chain.params conf in 
		let chain_thread = Thread.create chain_job bc in
		let net_thread = Thread.create net_job (bc, n, conf) in {
      chain= bc;
      net= n;
      chain_thread= chain_thread;
      net_thread= net_thread;
    }
;;


let stop lc =
	Log.info "letchain" "Stopping network thread...";
	Thread.kill lc.net_thread;
	Thread.join lc.net_thread;
	Log.info "letchain" "Network thread stopped.";
	Log.info "letchain" "Stopping chain thread...";	
	Thread.kill lc.chain_thread;
	Thread.join lc.chain_thread;
	Log.info "letchain" "Chain thread stopped.";
	true
;;


let is_synchronized lc = lc.chain.sync;;

let get_height lc = lc.chain.block_height;;

let get_last_block lc = lc.chain.block_last;;

let get_header_height lc = lc.chain.header_height;;

let get_last_heaer lc = lc.chain.header_last;;

let push_tx lc tx = false;;

let get_utxo lc h n = Storage.get_utx lc.chain.storage h n;;