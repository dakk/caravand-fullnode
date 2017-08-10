type t = {
	peers	 	: int;
	chain		: string;
	base_path: string;
	path		: string;
	api_port	: int;
	log_peer	: bool;
}

val parse_base_path			: unit -> string
val load_or_init				:	string -> t
val parse_command_line	: t -> t