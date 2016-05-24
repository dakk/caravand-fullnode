module In : sig
	type t = {
		tx 		: string;
		n		: int;
		script	: string;
		seq		: int32;	
	}
	
	val parse 	: bytes -> t
end

module Out : sig
	type t = {
		value	: int64;
		script	: string;	
	}
	
	val parse	: bytes -> t
end


type t = {
	version		: int32;
	txin 		: In.t list;
	txout 		: Out.t list;
	locktime	: int32;
}

val parse 		: bytes -> t