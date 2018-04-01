open Thread;;
open Blockchain;;
open Network;;
open Utils;;
open Bitcoinml;;
open Unix;;

type t = {
  chain       : Chain.t;
  net         : Net.t;
  chain_thread: Thread.t;
	net_thread  : Thread.t;
	params			: Params.t;
	conf				: Config.t;
};;

let init ?directory:(directory="") ?network:(network="XBT") ?peers:(peers=6) ?loglevel:(loglevel=0) mode = 
	let directory = if directory = "" then (Unix.getenv "HOME") ^ "/.letchain/" else directory in
	
	Log.set_level loglevel;
	Random.self_init ();
	Log.info Constants.name "Starting %s" Constants.version;
	let conf = { (directory |> Config.load_or_init) with peers=peers; log_level=loglevel; mode=mode } in
 	let cn = Params.abbr_to_network conf.chain in
	if cn = NOTFOUND then
		Log.info Constants.name "Invalid chain"
	else 	
		Log.info Constants.name "Selected network: %s" (Params.name_of_network cn);
		let p = Params.of_network cn in	
		
		let bc = Chain.load conf.path conf p in
		let n = Net.init bc.Chain.params conf in 
		let chain_thread = Thread.create (fun bc -> Chain.loop bc) bc in
		let net_thread = Thread.create (fun (n, bc) -> Net.loop n bc) (n, bc) in {
      chain= bc;
      net= n;
      chain_thread= chain_thread;
			net_thread= net_thread;
			params= p;
			conf= conf;
    }
;;


let stop lc =
	Log.info Constants.name "Stopping network thread...";
	Thread.kill lc.net_thread;
	Thread.join lc.net_thread;
	Log.info Constants.name "Network thread stopped.";
	Log.info Constants.name "Stopping chain thread...";	
	Thread.kill lc.chain_thread;
	Thread.join lc.chain_thread;
	Log.info Constants.name "Chain thread stopped.";
	true
;;

let get_network lc = lc.params.network;;

let get_directory lc = lc.conf.base_path;;

let join_threads lc = 
	Thread.join lc.chain_thread;
	Thread.join lc.net_thread
;;


let is_synchronized lc = lc.chain.sync;;

let get_height lc = lc.chain.block_height;;

let get_last_block lc = lc.chain.block_last;;

let get_header_height lc = lc.chain.header_height;;

let get_last_heaer lc = lc.chain.header_last;;

let push_tx lc tx = false;;

let get_utxo lc h n = Storage.get_utx lc.chain.storage h n;;