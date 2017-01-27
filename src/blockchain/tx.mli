open Stdint

module In : sig
	type t = {
		out_hash: string;
		out_n	: uint32;
		script	: Script.t;
		sequence: uint32;	
	}
	
	val parse 		: bytes -> bytes * t option
	val parse_all	: bytes -> bytes * t list
	val serialize	: t -> bytes
end

module Out : sig
	type t = {
		value	: int64;
		script	: Script.t;	
	}
	
	val parse		: bytes -> bytes * t option
	val parse_all	: bytes -> bytes * t list
	val serialize	: t -> bytes
end


type t = {
	hash		: Hash.t;
	version		: int32;
	txin 		: In.t list;
	txout 		: Out.t list;
	locktime	: uint32;
}

val parse 		: bytes -> bytes * t option
val serialize	: t -> bytes