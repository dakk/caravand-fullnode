open Yojson.Basic.Util;;

type t = {
	peers 		: int;
	chain		: string;
	path		: string;
};;


let parse_command_line conf =
	let help conf = 
		Printf.printf "Usage:\n"; 
		Printf.printf " -h, --help\t\t\tShow this help\n"; 
		Printf.printf " -c XTN|BTC, --chain XTN|BTC\tSelect the chain\n%!"; 
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
		| "-c" -> 
			Log.debug "Config" "Setting chain from command line: %s" x';
			parse ({ conf with 
				chain=x';
				path= (Unix.getenv "HOME" ^ "/.letchain/" ^ x')
			}) xl'
		| x -> parse conf (x'::xl')
	in parse conf (Array.to_list Sys.argv)
;;

let rec load_or_init () = 
	try
		let json = Yojson.Basic.from_file (Unix.getenv "HOME" ^ "/.letchain/config.json") in
		Log.info "Config" "Loaded %s" (Unix.getenv "HOME" ^ "/.letchain/config.json");
		let conf = {
			peers= json |> member "peers" |> to_int;
			chain= json |> member "chain" |> to_string;
			path= (Unix.getenv "HOME" ^ "/.letchain/" ^ (json |> member "chain" |> to_string));
		} in
		try
			Unix.mkdir (Unix.getenv "HOME" ^ "/.letchain/" ^ conf.chain) 0o777;
			Log.debug "Config" "Created %s" (Unix.getenv "HOME" ^ "/.letchain/" ^ conf.chain);
			conf
		with
		| _ -> conf			
	with
	| _ -> 
		try
			let json = Yojson.Basic.from_string "{ \"peers\": 8, \"chain\": \"XTN\" }" in 
			let _ = Yojson.Basic.to_file (Unix.getenv "HOME" ^ "/.letchain/config.json") json in
			Log.debug "Config" "Created %s" (Unix.getenv "HOME" ^ "/.letchain/config.json");
			load_or_init ()
		with
		| _ -> 
			Unix.mkdir (Unix.getenv "HOME" ^ "/.letchain/") 0o777;
			Log.debug "Config" "Created directory %s" (Unix.getenv "HOME" ^ "/.letchain/");
			load_or_init ()
;;