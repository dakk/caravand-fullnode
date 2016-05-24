module In = struct 
	type t = {
		tx 		: string;
		n		: int;
		script	: string;
		seq		: int32;	
	};;

	let parse data = {
		tx= "";
		n= 1;
		script= "";
		seq= Int32.of_int 12;
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
	version		: int32;
	txin 		: In.t list;
	txout 		: Out.t list;
	locktime	: int32;
};;


let parse data = {
	version= Int32.of_int 0;
	txin= [];
	txout= [];
	locktime= Int32.of_int 0;
};;







