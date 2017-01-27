open Stdint;;

module In = struct 
	type t = {
		tx 		: string;
		n		: uint32;
		script	: Script.t;
		seq		: uint32;	
	};;

	let serialize txin = "";;

	let parse data = Some {
		tx= "";
		n= Uint32.of_int 0;
		script= "";
		seq= Uint32.of_int 12;
	};;
end

module Out = struct 
	type t = {
		value	: int64;
		script	: Script.t;	
	};;

	let serialize txout = "";;
	
	let parse data = Some {
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


let parse data = Some {
	hash	= Crypto.hash256 data;
	version	= Int32.of_int 0;
	txin	= [];
	txout	= [];
	locktime= Uint32.of_int 0;
};;


let serialize tx = "";;