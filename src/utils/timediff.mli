type t = {
	years 	: int;
	months	: int;
	days  	: int;
	hours 	: int;
	minutes	: int;
}

val diff 		: float -> float -> t
val diffstring 	: ?munit:string -> float -> float -> string