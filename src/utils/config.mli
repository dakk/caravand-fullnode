type node_type = 
	| FullNode            (* Node with full block data*)
	| PrunedNode of int   (* Node with full block data of last n blocks (Address disabled) *)
	| HeadersOnly         (* Node with only headers *)

type t = {
	peers	 		: int;
	chain			: string;
	base_path	: string;
	path			: string;
	api_port	: int;

	address_index	: bool;
	tx_index			: bool;
	mode					: node_type;
	log_level			: int;
}

val parse_base_path			: unit -> string
val load_or_init				:	string -> t
val parse_command_line	: t -> t