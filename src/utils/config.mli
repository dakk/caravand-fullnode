type t = {
	min_peers 	: int;
	max_peers 	: int;
	chain		: string;
	user_agent	: string;
}

val from_file : string -> t