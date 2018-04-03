open Network;;
open Utils;;
open Blockchain;;
open Thread;;
open Random;;
open Config;;
open Bitcoinml;;

let main () =
	Random.self_init ();
	Log.info Constants.name "Starting %s" Constants.version;
	let conf = Config.parse_base_path () |> Config.load_or_init |> Config.parse_command_line in
	let _ = Config.create_dirs conf in
	let cn = Params.abbr_to_network conf.chain in
	Log.set_level conf.log_level;
	if cn = NOTFOUND then
		Log.info Constants.name "Invalid chain"
	else 	
		Log.info Constants.name "Selected network: %s" (Params.name_of_network cn);
		let p = Params.of_network cn in	
				
		let bc = Chain.load conf.path conf p in
		let n = Net.init bc.Chain.params conf in 
		let chain_thread = Thread.create (fun bc -> Chain.loop bc) bc in
		let net_thread = Thread.create (fun (n, bc) -> Net.loop n bc) (n, bc) in

		let rest = Api.Rest.init conf.rest bc n in
		let rest_thread = Thread.create (fun rest -> Api.Rest.loop rest) rest in
		let rpc = Api.Rpc.init conf.rpc bc n in
		let rpc_thread = Thread.create (fun rpc -> Api.Rpc.loop rpc) rpc in
		
		let sighandler signal =
			Log.fatal Constants.name "Quit signal, shutdown. Please wait for the secure shutdown procedure.";
			Net.shutdown n;
			Api.Rest.shutdown rest;
			Api.Rpc.shutdown rpc;
			Chain.shutdown bc
		in

		(*Sys.set_signal Sys.sigint @@ Signal_handle (sighandler);*)
		Sys.set_signal Sys.sigint @@ Signal_handle (sighandler);

		Log.info Constants.name "Waiting for childs";

		Thread.join net_thread;
		Thread.join chain_thread;
		Thread.join rest_thread;
		Thread.join rpc_thread;
		Log.info Constants.name "Exit.";
;;

main ();;
