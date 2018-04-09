type node_type = 
	| FullNode            (* Node with full block data*)
	| PrunedNode of int   (* Node with full block data of last n blocks (Address disabled) *)
	| HeadersOnly         (* Node with only headers *)

type rest = {
	enable: bool;
	port	: int;
}
	
type rpc = {
	enable: bool;
	port	: int;
	user	: string;
	password: string;
}
	
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
}

val parse_base_path			: unit -> string
val load_or_init				:	string -> t
val parse_command_line	: t -> t
val create_dirs					: t -> bool