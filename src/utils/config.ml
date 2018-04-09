open Yojson.Basic.Util;;

type node_type = 
	| FullNode            (* Node with full block data*)
	| PrunedNode of int   (* Node with full block data of last n blocks (Address disabled) *)
	| HeadersOnly         (* Node with only headers *)
;;

type rest = {
	enable: bool;
	port	: int;
};;

type rpc = {
	enable: bool;
	port	: int;
	user	: string;
	password: string;
};;

type t = {
	peers	 		: int;
	chain			: string;
	base_path	: string;
	path			: string;

	rest			: rest;
	rpc				: rpc;

	address_index	: bool;
	tx_index			: bool;
	mode					: node_type;
	log_level			: int;
};;

let parse_base_path () = 
	let rec parse' argvs = match argvs with
	| [] -> (Unix.getenv "HOME") ^ "/." ^ Constants.name_lower
	| x :: [] -> (Unix.getenv "HOME") ^ "/." ^ Constants.name_lower
	| x :: x' :: xl' -> (
		match x with
		| "--data-dir"
		| "-d" ->
			Log.debug "Config" "Setting data dir from command line: %s" x';
			x'
		| _ -> parse' (x' :: xl')
	) in parse' (Array.to_list Sys.argv)
;;

let parse_command_line conf =
	let bool_of_string s = match s with
	| "true" -> true
	| _ -> false
	in
	let help conf = 
		Printf.printf "Usage:\n"; 
		Printf.printf " -h, --help\t\t\tShow this help\n"; 
		Printf.printf " -c XTN|BTC, --chain XTN|BTC\tSelect the chain\n"; 
		Printf.printf " -p 12, --peer 12\tSet the number of peers\n"; 
		Printf.printf " -r 1024, --prune 1024\tPrune to a custom number of blocks\n"; 
		Printf.printf " -d /path/, --data-dir /path/\tSelect the destination directory for data\n"; 
		Printf.printf " -rp 8087, --rest-port 8086\t\t\tSelect the rest api port\n%!";  
		Printf.printf " -rpp 8087, --rpc-port 8087\t\t\tSelect the rpc api port\n%!";  
		Printf.printf " -ll 5, --log-level 5\tSet the log level\n%!";  
		Printf.printf " -ho, --header-only\tDownload and sync only headers\n%!";
		Printf.printf " -ai, --address-index\tEnable address index (only on first run)\n%!";
		Thread.exit (); 
		conf
	in
	let rec parse conf argvs = match argvs with
	| [] -> conf
	| x :: [] -> (
		match x with
		| "--help" 
		| "-h" -> help conf	
		| "--address-index"
		| "-ai" -> 
			Log.debug "Config" "Enabling address index";
			{ conf with address_index=true }
		| "--headers-only"
		| "-ho" -> 
			Log.debug "Config" "Setting mode to headers-only";
			{ conf with mode=HeadersOnly }
		| _ -> conf
	)
	| x :: x' :: xl' -> 
		match x with
		| "--help"
		| "-h" -> help conf
		| "--address-index"
		| "-ai" -> 
			Log.debug "Config" "Enabling address index";
			{ conf with address_index=true }
		| "--headers-only"
		| "-ho" -> 
			Log.debug "Config" "Setting mode to headers-only";
			parse ({ conf with mode=HeadersOnly }) xl'
		| "--rest-port"
		| "-rp" -> 
			Log.debug "Config" "Setting rest api port from command line: %s" x';
			parse ({ conf with rest={conf.rest with port= int_of_string x' }}) xl'	
		| "--rpc-port"
		| "-rpp" -> 
			Log.debug "Config" "Setting rpc api port from command line: %s" x';
			parse ({ conf with rpc={conf.rpc with port= int_of_string x' }}) xl'	
		| "--log-level"
		| "-ll" -> 
			Log.debug "Config" "Setting the log level from command line: %s" x';
			parse ({ conf with log_level= int_of_string x' }) xl'
		| "--peer"
		| "-p" -> 
			Log.debug "Config" "Setting peer number to: %s" x';
			parse ({ conf with peers= int_of_string x' }) xl'
		| "--prune" 
		| "-r" -> 
			Log.debug "Config" "Setting the prune size to: %s blocks" x';
			let nblocks = int_of_string x' in
			if nblocks < 1024 then (
				Log.error "Config" "Pruned blocks must be greater or equal to 1024";
				failwith "Config error")
			else
				parse ({ conf with mode= PrunedNode (int_of_string x') }) xl'
		| "--chain"
		| "-c" -> 
			Log.debug "Config" "Setting chain from command line: %s" x';
			parse ({ conf with chain=x'; path= conf.base_path ^ "/" ^ x' }) xl'
		| x -> parse conf (x'::xl')
	in parse conf (Array.to_list Sys.argv)
;;

let create_dirs conf =
	try
		Unix.mkdir (conf.base_path ^ "/" ^ conf.chain) 0o777;
		Unix.mkdir (conf.base_path ^ "/" ^ conf.chain ^ "/blocks") 0o777;
		Unix.mkdir (conf.base_path ^ "/" ^ conf.chain ^ "/state") 0o777;
		Unix.mkdir (conf.base_path ^ "/" ^ conf.chain ^ "/address") 0o777;
		Log.debug "Config" "Created %s [/blocks, /state, /address]" (conf.base_path ^ "/" ^ conf.chain);
		true
	with
	| _ -> false
;;

let rec load_or_init base_path = 
	try
		Unix.mkdir base_path 0o777;
		Log.debug "Config" "Created directory %s" base_path;
		load_or_init base_path
	with
	| _ -> 
		try
			let json = Yojson.Basic.from_file (base_path ^ "/config.json") in
			Log.info "Config" "Loaded %s" (base_path ^ "/config.json");
			{
				base_path= base_path;
				peers= json |> member "peers" |> to_int;
				chain= json |> member "chain" |> to_string;
				
				rest= {
					port= json |> member "rest" |> member "port" |> to_int;
					enable= json |> member "rest" |> member "enable" |> to_bool;
				};

				rpc= {
					enable= json |> member "rpc" |> member "enable" |> to_bool;
					port= json |> member "rpc" |> member "port" |> to_int;
					user= base_path ^ (json |> member "rpc" |> member "user" |> to_string);
					password= base_path ^ (json |> member "rpc" |> member "password" |> to_string);
				};
				
				path= base_path ^ (json |> member "chain" |> to_string);
				address_index= json |> member "address_index" |> to_bool;
				tx_index= true;
				mode= FullNode;
				log_level= 4;
			}
		with
		| _ ->
			let (jconf: Yojson.Basic.json) = `Assoc [
				("peers", `Int 6);
				("chain", `String "XTN");
				("address_index", `Bool false);
				("rest", `Assoc [
					("enable", `Bool true);
					("port", `Int 8086)
				]);
				("rpc", `Assoc [
					("enable", `Bool false);
					("port", `Int 8332);
					("user", `String "test");
					("password", `String "test")
				])
			] in
			let _ = Yojson.Basic.to_file (base_path ^ "/config.json") jconf in
			Log.debug "Config" "Created %s" (base_path ^ "/config.json");
			load_or_init base_path
;;