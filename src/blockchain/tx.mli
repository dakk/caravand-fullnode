open Stdint

module In : sig
	type t = {
		tx 		: string;
		n		: uint32;
		script	: string;
		seq		: uint32;	
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
	hash		: Hash.t;
	version		: int32;
	txin 		: In.t list;
	txout 		: Out.t list;
	locktime	: uint32;
}

val parse 		: bytes -> t
val hash		: t -> Hash.t
val serialize	: t -> bytes