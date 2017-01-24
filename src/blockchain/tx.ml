open Stdint;;

module In = struct 
	type t = {
		tx 		: string;
		n		: uint32;
		script	: string;
		seq		: uint32;	
	};;

	let parse data = {
		tx= "";
		n= Uint32.of_int 0;
		script= "";
		seq= Uint32.of_int 12;
	};;
end

module Out = struct 
	type t = {
		value	: int64;
		script	: string;	
	};;
	
	let parse data = {
		value= Int64.of_int 0;
		script= "";
	};;
end


type t = {
	hash		: Hash.t;
	version		: int32;
	txin 		: In.t list;
	txout 		: Out.t list;
	locktime	: uint32;
};;


let parse data = {
	hash	= Crypto.hash256 data;
	version	= Int32.of_int 0;
	txin	= [];
	txout	= [];
	locktime= Uint32.of_int 0;
};;





let serialize tx = "";;