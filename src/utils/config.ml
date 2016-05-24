type t = {
	min_peers 	: int;
	max_peers 	: int;
	chain		: string;
	user_agent	: string;
};;

let from_file path =
	{
		min_peers= 4;
		max_peers= 25;
		chain= "BTC";
		user_agent= "/letchain:0.12.1/";
	}
;;