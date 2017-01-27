open Stdint

module In : sig
	type t = {
		tx 		: string;
		n		: uint32;
		script	: Script.t;
		seq		: uint32;	
	}
	
	val parse 		: bytes -> t option
	val serialize	: t -> bytes
end

module Out : sig
	type t = {
		value	: int64;
		script	: Script.t;	
	}
	
	val parse		: bytes -> t option
	val serialize	: t -> bytes
end


type t = {
	hash		: Hash.t;
	version		: int32;
	txin 		: In.t list;
	txout 		: Out.t list;
	locktime	: uint32;
}

val parse 		: bytes -> t option
val serialize	: t -> bytes