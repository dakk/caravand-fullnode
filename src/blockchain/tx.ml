module In = struct 
	type t = {
		tx 		: string;
		out_n	: int;
		script	: string;
		seq		: int32;	
	};;
end

module Out = struct 
	type t = {
		value	: int64;
		script	: string;	
	};;
end


type t = {
	version		: int32;
	txin 		: In.t list;
	txout 		: Out.t list;
	locktime	: int32;
};;









