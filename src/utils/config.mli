type t = {
	peers	 	: int;
	chain		: string;
	path		: string;
}

val load_or_init	:	unit -> t