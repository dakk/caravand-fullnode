open Network;;
open Utils;;
open Blockchain;;
open Thread;;
open Random;;
open Config;;
open Bitcoinml;;

let main () =
	let chain_job bc = 
		Chain.loop bc 
	in
	let net_job (bc, net, conf) = 
		Net.loop net bc
	in
	let rest_job (rest) =
		Api.Rest.loop rest
	in
	
	Random.self_init ();
	Log.info "letchain" "Starting 0.1";
	let conf = Config.parse_base_path () |> Config.load_or_init |> Config.parse_command_line in
	let _ = Config.create_dirs conf in
	let cn = Params.abbr_to_network conf.chain in
	Log.set_level conf.log_level;
	if cn = NOTFOUND then
		Log.info "letchain" "Invalid chain"
	else 	
		Log.info "letchain" "Selected network: %s" (Params.name_of_network cn);
		let p = Params.of_network cn in	
				
		let bc = Chain.load conf.path conf p in
		let n = Net.init bc.Chain.params conf in 
		let chain_thread = Thread.create chain_job bc in
		let net_thread = Thread.create net_job (bc, n, conf) in

		let rest = Api.Rest.init conf.rest bc n in
		let rest_thread = Thread.create rest_job (rest) in

		
		let sighandler signal =
			Log.fatal "letchain" "Quit signal, shutdown. Please wait for the secure shutdown procedure.";
			Net.shutdown n;
			Api.Rest.shutdown rest;
			Chain.shutdown bc
		in

		(*Sys.set_signal Sys.sigint @@ Signal_handle (sighandler);*)
		Sys.set_signal Sys.sigint @@ Signal_handle (sighandler);

		Log.info "letchain" "Waiting for childs";
		Thread.join net_thread;
		Thread.join chain_thread;
		Thread.join rest_thread;
		Log.info "letchain" "Exit.";
;;

main ();;
