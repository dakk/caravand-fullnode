open Yojson.Basic.Util;;

type node_type = 
	| FullNode            (* Node with full block data*)
	| PrunedNode of int   (* Node with full block data of last n blocks (Address disabled) *)
	| HeadersOnly         (* Node with only headers *)
;;

type t = {
	peers	 		: int;
	chain			: string;
	base_path	: string;
	path			: string;
	api_port	: int;
	log_peer	: bool;

	address_index	: bool;
	tx_index			: bool;
	mode					: node_type;
};;

let parse_base_path () = 
	let rec parse' argvs = match argvs with
	| [] -> (Unix.getenv "HOME") ^ "/.letchain/"
	| x :: [] -> (Unix.getenv "HOME") ^ "/.letchain/"
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
		Printf.printf " -ap 807\t\t\tSelect api port\n%!";  
		Printf.printf " --log-peer true\t\t\tEnable log for peer mesages\n%!"; 
		Thread.exit (); 
		conf
	in
	let rec parse conf argvs = match argvs with
	| [] -> conf
	| x :: [] -> (
		match x with
		| "--help" 
		| "-h" -> help conf
		| _ -> conf
	)
	| x :: x' :: xl' -> 
		match x with
		| "--help"
		| "-h" -> help conf
		| "-ap" -> 
			Log.debug "Config" "Setting api port from command line: %s" x';
			parse ({ conf with 
				api_port= int_of_string x'
			}) xl'
		| "--log-peer" ->
			parse ({ conf with 
				log_peer= bool_of_string x'
			}) xl'
			| "--peer"
			| "-p" -> 
				Log.debug "Config" "Setting peer number to: %s" x';
				parse ({ conf with 
					peers= int_of_string x'
				}) xl'
		| "--prune"
		| "-r" -> 
			Log.debug "Config" "Setting the prune size to: %s blocks" x';
			let nblocks = int_of_string x' in
			if nblocks < 1024 then (
				Log.error "Config" "Pruned blocks must be greater or equal to 1024";
				failwith "Config error")
			else
				parse ({ conf with 
					mode= PrunedNode (int_of_string x')
				}) xl'
		| "--chain"
		| "-c" -> 
			Log.debug "Config" "Setting chain from command line: %s" x';
			parse ({ conf with 
				chain=x';
				path= conf.base_path ^ "/" ^ x'
			}) xl'
		| x -> parse conf (x'::xl')
	in parse conf (Array.to_list Sys.argv)
;;

let rec load_or_init base_path = 
	try
		let json = Yojson.Basic.from_file (base_path ^ "/config.json") in
		Log.info "Config" "Loaded %s" (base_path ^ "/config.json");
		let conf = {
			base_path= base_path;
			peers= json |> member "peers" |> to_int;
			chain= json |> member "chain" |> to_string;
			api_port= json |> member "api_port" |> to_int;
			path= base_path ^ (json |> member "chain" |> to_string);
			log_peer= false;
			address_index= true;
			tx_index= true;
			mode= FullNode;
		} in
		try
			Unix.mkdir (base_path ^ "/" ^ conf.chain) 0o777;
			Log.debug "Config" "Created %s" (base_path ^ "/" ^ conf.chain);
			conf
		with
		| _ -> conf			
	with
	| _ -> 
		try
			let json = Yojson.Basic.from_string "{ \"peers\": 6, \"chain\": \"XTN\", \"api_port\": 8086 }" in 
			let _ = Yojson.Basic.to_file (base_path ^ "/config.json") json in
			Log.debug "Config" "Created %s" (base_path ^ "/config.json");
			load_or_init base_path
		with
		| _ -> 
			Unix.mkdir base_path 0o777;
			Log.debug "Config" "Created directory %s" base_path;
			load_or_init base_path
;;