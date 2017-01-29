open Yojson.Basic.Util;;

type t = {
	peers 	: int;
	chain		: string;
	path		: string;
};;


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
			let json = Yojson.Basic.from_string "{ \"peers\": 8, \"chain\": \"XBT\" }" in 
			let _ = Yojson.Basic.to_file (Unix.getenv "HOME" ^ "/.letchain/config.json") json in
			Log.debug "Config" "Created %s" (Unix.getenv "HOME" ^ "/.letchain/config.json");
			load_or_init ()
		with
		| _ -> 
			Unix.mkdir (Unix.getenv "HOME" ^ "/.letchain/") 0o777;
			Log.debug "Config" "Created directory %s" (Unix.getenv "HOME" ^ "/.letchain/");
			load_or_init ()
;;