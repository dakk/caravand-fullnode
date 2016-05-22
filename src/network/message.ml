open Bitstring;;
open Params;;

type header = {
	magic		: int32;
	command		: string;
	length		: int32;
	checksum	: string;
};;

type version = {
	version		: int32;
	services	: int64;
	timestamp	: Unix.tm;
	addr_recv	: string;
	addr_from	: string;
	nonce		: int64;
	user_agent	: string;
	start_height: int32;
	relay		: bool;
};;


type t = 
	  VERSION of version
	| VERACK
	| PING
	| PONG
	| INV
	| ADDR
	| GETDATA
	| NOTFOUND
	| GETBLOCKS
	| GETHEADERS
	| TX
	| BLOCKS
	| HEADERS
	| GETADDR
	| MEMPOOL
	| REJECT
	
	(* Bloom filter related *)
	| FILTERLOAD
	| FILTERADD
	| FILTERCLEAR
	| MERKLEBLOCK
	
	| ALERT
	| SENDHEADERS
;;



(******************************************************************)
(* Parsing ********************************************************)
(******************************************************************)
let parse_header data =
	let bdata = bitstring_of_string data in
	bitmatch bdata with
	| { 
		magic 		: 4*8 	: littleendian;
		command 	: 12*8 	: string;
		length 		: 4*8 	: littleendian;
		checksum	: 4*8 	: string
	} ->
	{
		magic 		= magic;
		command 	= command;
		length 		= length;
		checksum	= checksum;
		}
	| { _ } -> raise (Invalid_argument "Invalid protocol header")
;;


let parse header payload = 
	match header.command with
	| "version" -> VERACK
	| "ping" -> VERACK
	| "pong" -> VERACK
	| _ -> raise (Invalid_argument "Protocol command not recognized")
;;








(******************************************************************)
(* Serialization **************************************************)
(******************************************************************)

let serialize_header header =
	let bdata = BITSTRING {
		header.magic 	: 4*8 	: littleendian;
		header.command	: 12*8 	: string;
		header.length 	: 4*8 	: littleendian;
		header.checksum : 4*8 	: string
	} 
	in string_of_bitstring bdata
;;


let serialize_message message = 
	let bdata = match message with
	| VERSION (v) -> empty_bitstring
	| VERACK -> empty_bitstring
	| _ -> empty_bitstring
	in string_of_bitstring bdata
;;

let serialize params message = 
	let mdata = serialize_message message in
	let header = {
		magic	= Int32.of_int params.Params.magic;
		command	= "ping        ";
		length	= Int32.of_int (Bytes.length mdata);
		checksum= "1234";
	} in 
	let hdata = serialize_header header in
	Bytes.cat hdata mdata
;;