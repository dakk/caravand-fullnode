module Header = struct
	type t = {
		hash		: Hash.t;
		version		: int32;
		prev_block	: Hash.t;
		merkle_root : Hash.t;
		timestamp	: float;
		bits		: int32;
		nonce		: int32;
		txn			: int64;	
	};;
	
	let parse data = {
		hash= data;
		version= Int32.of_int 0;
		prev_block= "";
		merkle_root= "";
		timestamp= 0.0;
		bits= Int32.of_int 12;
		nonce= Int32.of_int 12;
		txn= Int64.of_int 12;
	};;
end

type t = {
	header	: Header.t;
	txs		: Tx.t list;
};;


let parse data = {
	header= Header.parse data;
	txs= [];
};;


let hash block = "";;

let serialize block = "";