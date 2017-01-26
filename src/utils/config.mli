type t = {
	min_peers 	: int;
	max_peers 	: int;
	chain		: string;
}

val load_or_init	:	unit -> t