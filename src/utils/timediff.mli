type t = {
	years 	: int;
	months	: int;
	days  	: int;
	hours 	: int;
	minutes	: int;
}

val diff 		: float -> float -> t
val diffstring 	: float -> float -> string