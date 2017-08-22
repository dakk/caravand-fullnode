open Thread;;
open Blockchain;;
open Network;;
open Utils;;
open Unix;;

type node_type = 
  | FullNode            (* Node with full block data*)
  | PrunedNode of int   (* Node with full block data of last n blocks (Address disabled) *)
  | HeadersOnly         (* Node with only headers *)
;;

type t = {
  chain       : Chain.t;
  net         : Net.t;
  chain_thread: Thread.t;
  net_thread  : Thread.t;
};;

let init ?directory ?network:(network="XBT") ?peers:(peers=6) ?loglevel:(loglevel=0) mode = 
	let chain_job bc = Chain.loop bc in
	let net_job (bc, net, conf) = Net.loop net bc in
	
	Log.set_level loglevel;
	Random.self_init ();
	Log.info "letchain" "Starting 0.1";
	let conf = Config.parse_base_path () |> Config.load_or_init |> Config.parse_command_line in
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
