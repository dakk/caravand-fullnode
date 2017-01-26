open Yojson.Basic.Util;;

type t = {
	min_peers 	: int;
	max_peers 	: int;
	chain		: string;
};;


let rec load_or_init () = 
	try
		let json = Yojson.Basic.from_file (Unix.getenv "HOME" ^ "/.letchain/config.json") in
		Log.info "Config" "Loaded %s" (Unix.getenv "HOME" ^ "/.letchain/config.json");
		{
			min_peers= json |> member "min_peers" |> to_int;
			max_peers= json |> member "max_peers" |> to_int;
			chain= json |> member "chain" |> to_string;
		}
	with
	| _ -> 
		try
			let json = Yojson.Basic.from_string "{ \"min_peers\": 4, \"max_peers\": 16, \"chain\": \"XBT\" }" in 
			let _ = Yojson.Basic.to_file (Unix.getenv "HOME" ^ "/.letchain/config.json") json in
			Log.debug "Config" "Created %s" (Unix.getenv "HOME" ^ "/.letchain/config.json");
			load_or_init ()
		with
		| _ -> 
			Unix.mkdir (Unix.getenv "HOME" ^ "/.letchain/") 0o777;
			Log.debug "Config" "Created directory %s" (Unix.getenv "HOME" ^ "/.letchain/");
			load_or_init ()
;;