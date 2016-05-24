open Bitstring;;
open Params;;
open Crypto;;
open Stdint;;


type header = {
	magic		: int32;
	command		: string;
	length		: uint32;
	checksum	: string;
};;


type invvect = 
	  INV_ERROR
	| INV_TX of Hash.t
	| INV_BLOCK of Hash.t
	| INV_FILTERED_BLOCK of Hash.t

type addr = {
	services	: int64;
	address		: string;
	port		: int
};;


type inv = invvect list;;

type version = {
	version		: int32;
	services	: int64;
	timestamp	: float;
	addr_recv	: addr;
	addr_from	: addr;
	nonce		: int64;
	user_agent	: string;
	start_height: int32;
	relay		: bool;
};;

type getheaders = {
	version		: int32;
	count		: int;
	hashes		: Hash.t list;
	stop		: Hash.t;
}

type getblocks = getheaders

type headers = Block.Header.t list

type ping = int64;;
type pong = int64;;


type t = 
	  VERSION of version
	| VERACK
	| PING of ping
	| PONG of pong
	| INV of inv
	| ADDR
	| GETDATA
	| NOTFOUND
	| GETBLOCKS of getblocks
	| GETHEADERS of getheaders
	| TX
	| BLOCKS
	| HEADERS of headers
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


let string_of_command c = match c with
	  VERSION (v) -> "version"
	| VERACK -> "verack"
	| PING (p) -> "ping"
	| PONG (p) -> "pong"
	| INV (i) -> "inv"
	| ADDR -> "addr"
	| GETDATA -> "getdata"
	| NOTFOUND -> "notfound"
	| GETBLOCKS (gb) -> "getblocks"
	| GETHEADERS (gh) -> "getheaders"
	| TX -> "tx"
	| BLOCKS -> "blocks"
	| HEADERS (h) -> "headers"
	| GETADDR -> "getaddr"
	| MEMPOOL -> "mempool"
	| REJECT -> "reject"
	
	(* Bloom filter related *)
	| FILTERLOAD -> "filterload"
	| FILTERADD -> "filteradd"
	| FILTERCLEAR -> "filterclear"
	| MERKLEBLOCK -> "merkleblock"
	
	| ALERT -> "alert"
	| SENDHEADERS -> "sendheaders"
;;




(******************************************************************)
(* Parsing ********************************************************)
(******************************************************************)
let string_from_zeroterminated_string zts =
	let string_length =
 		try String.index zts '\x00' with Not_found -> 12
	in String.sub zts 0 string_length
;;

let parse_varint bits =
	let parse_tag_byte bits =
		bitmatch bits with
		| { tag : 1*8 : littleendian; rest : -1 : bitstring } -> (tag, rest)
		| { _ } -> (0, bits)
	in
	let parse_value bits bytesize =
		bitmatch bits with
		| { value : bytesize*8 : littleendian; rest : -1 : bitstring } -> (value, rest)
		| { _ } -> (Int64.of_int 0, bits)
	in
	let tag, rest = parse_tag_byte bits in
		match tag with
		| 0xff -> parse_value rest 8
		| 0xfe -> parse_value rest 4
		| 0xfd -> parse_value rest 2
		| x -> (Int64.of_int x, rest)
;;


let parse_varstring bits =
  let length, bits = parse_varint bits in
  match length with
  | 0L -> ("", bits)
  | length ->
    bitmatch bits with
    | { value : (Int64.to_int length) * 8 : string;
        rest : -1 : bitstring
      } -> (value, rest)
    | { _ } -> ("", bits)
;;


let parse_inv data =
	let rec parse_invvects bdata count acc = 
		if count = 0 then acc else (
		bitmatch bdata with 
		| { 
			itype		: 4*8 : littleendian;
			hash		: 32*8: string;
			rest		: -1  : bitstring
		} ->
			let iv = match (Int32.to_int itype) with
				  1 -> INV_TX  (hash)
				| 2 -> INV_BLOCK (hash)
				| 3 -> INV_FILTERED_BLOCK (hash)
				| _ -> INV_ERROR
			in parse_invvects rest (count - 1) (iv::acc)
		)
	in
	let bdata = bitstring_of_string data in
	let count, rest = parse_varint bdata in
	parse_invvects rest (Int64.to_int count) []
;;

let parse_version data =
	let bdata = bitstring_of_string data in
	bitmatch bdata with 
	| {
		version 			: 4*8 : littleendian;
		services 			: 8*8 : littleendian;
		timestamp 			: 8*8 : littleendian;
		addr_recv			: 26*8 : bitstring;
		addr_from	 		: 26*8 : bitstring;
		nonce				: 8*8 : littleendian;
		rest				: -1 : bitstring
	} -> 
		match parse_varstring rest with
		| (user_agent, rest) ->
			(bitmatch rest with 
			| {
				start_height		: 4*8 : littleendian;
				relay				: 1*8 : littleendian						
			} -> {
				addr_recv= { address="0000000000000000" ; services=(Int64.of_int 1) ; port= 8333 };
				addr_from= { address="0000000000000000" ; services=(Int64.of_int 1) ; port= 8333 };
				version= version;
				services= services;
				timestamp= Int64.to_float (timestamp);
				nonce= nonce;
				user_agent= user_agent;
				start_height= start_height;
				relay= false
			}
			)
;;

let parse_ping data =
	let bdata = bitstring_of_string data in
	bitmatch bdata with
	| { nonce		: 8*8	: littleendian } -> nonce
	| { _ } -> raise (Invalid_argument "Invalid ping message")
;;

let parse_pong data =
	let bdata = bitstring_of_string data in
	bitmatch bdata with
	| { nonce		: 8*8	: littleendian } -> nonce
	| { _ } -> raise (Invalid_argument "Invalid pong message")
;;


let parse_headers data = 
	let rec ph' data n acc =
		if n = 0 then acc else
			bitmatch data with
			| { raw : 81*8 : bitstring; rest : -1 : bitstring } ->
				ph' rest (n-1) ((Block.Header.parse (string_of_bitstring raw))::acc)
	in  
	let bdata = bitstring_of_string data in
	let count, rest = parse_varint bdata in
	ph' (bdata) (Int64.to_int count) []
;;

let parse_header data =
	let bdata = bitstring_of_string data in
	bitmatch bdata with
	| { 
		magic 		: 4*8 	: littleendian;
		command 	: 12*8 	: string;
		length 		: 4*8 	: string;
		checksum	: 4*8 	: string
	} ->
	{
		magic 		= magic;
		command 	= string_from_zeroterminated_string command;
		length 		= Uint32.of_bytes_little_endian length 0;
		checksum	= checksum;
		}
	| { _ } -> raise (Invalid_argument "Invalid protocol header")
;;


let parse header payload = 
	match header.command with
	| "version" -> VERSION (parse_version payload)
	| "ping" -> PING (parse_ping payload)
	| "pong" -> PONG (parse_pong payload)
	| "headers" -> HEADERS (parse_headers payload)
	| "verack" -> VERACK
	| "getaddr" -> GETADDR
	| "mempool" -> MEMPOOL
	| "sendheaders" -> SENDHEADERS
	(*| "getheaders" -> GETHEADERS *)
	| "inv" -> INV (parse_inv payload)
	| "addr" -> ADDR
	| _ -> raise (Invalid_argument ("Protocol command " ^ header.command ^ " not recognized"))
;;







(******************************************************************)
(* Serialization **************************************************)
(******************************************************************)
let bitstring_of_addr (addr: addr) : Bitstring.t =
  BITSTRING {
    addr.services	: 8*8 	: littleendian;
    addr.address	: 16*8 	: string;
    addr.port		: 2*8 	: bigendian
  }
;;

let bitstring_of_int i = 
	match i with
	| i when i < 0xFDL -> BITSTRING { Int64.to_int i : 1*8 : littleendian }
	| i when i < 0xFFFFL -> BITSTRING { 0xFD : 1*8; Int64.to_int i : 2*8 : littleendian }
	| i when i < 0xFFFFFFFFL -> BITSTRING { 0xFE : 1*8; Int64.to_int32 i : 4*8 : littleendian }
	| i -> BITSTRING { 0xFF : 1*8; i : 8*8 : littleendian }
;;

let bitstring_of_varstring s = 
	match String.length s with
	| 0 -> bitstring_of_string "\x00"
	| n -> 
		let length_varint_bitstring = bitstring_of_int (Int64.of_int (String.length s)) in
		BITSTRING {
			length_varint_bitstring : -1 : bitstring;
			s 						: (String.length s) * 8 : string
		}
;;

let int_of_bool b = 
	match b with
	| true -> 1
	| false -> 0
;;

let serialize_getheaders v = 
	BITSTRING {
		v.version 								: 4*8 : littleendian;
		bitstring_of_int (Int64.of_int v.count)	: -1 : bitstring;
		List.nth v.hashes 0						: 32*8 : string; (*TODO: this should be an array *)
		v.stop									: 32*8 : string
	} 
;;

let serialize_getblocks v = serialize_getheaders v;;

let serialize_version (v:version) =
	BITSTRING {
		v.version 										: 4*8 : littleendian;
		v.services 										: 8*8 : littleendian;
		Int64.of_float (v.timestamp) 					: 8*8 : littleendian;
		(bitstring_of_addr v.addr_recv)					: -1 : bitstring;
		(bitstring_of_addr v.addr_from)			 		: -1 : bitstring;
		v.nonce											: 8*8 : littleendian;
		bitstring_of_varstring v.user_agent 			: -1 : bitstring;
		v.start_height 									: 4*8 : littleendian;
		int_of_bool true								: 1*8 : littleendian
	}
;;

let serialize_ping p = BITSTRING { p 	: 8*8 : littleendian };;
let serialize_pong p = BITSTRING { p 	: 8*8 : littleendian };;

let serialize_header header =
	let blength = Bytes.create 4 in
	let _ = Uint32.to_bytes_little_endian header.length blength 0 in
	let bdata = BITSTRING {
		header.magic 	: 4*8 	: littleendian;
		header.command	: 12*8 	: string;
		blength			: 4*8 	: string;
		header.checksum : 4*8 	: string
	} 
	in string_of_bitstring bdata
;;


let serialize_message message = 
	let bdata = match message with
	| PING (p) -> serialize_ping p
	| PONG (p) -> serialize_pong p
	| VERSION (v) -> serialize_version v
	| VERACK -> empty_bitstring
	| GETHEADERS (gh) -> serialize_getheaders gh
	| GETBLOCKS (gb) -> serialize_getblocks gb
	
	| GETADDR -> empty_bitstring
	| MEMPOOL -> empty_bitstring
	| SENDHEADERS -> empty_bitstring
	
	| _ -> empty_bitstring
	in string_of_bitstring bdata
;;


let serialize params message = 
	let mdata = serialize_message message in
	let command = string_of_command message in
	let command' = command ^ (String.make (12 - (String.length command)) '\x00') in
	let header = {
		magic	= Int32.of_int params.Params.magic;
		command	= command';
		length	= Uint32.of_int (Bytes.length mdata);
		checksum= Crypto.checksum4 mdata;
	} in 
	let hdata = serialize_header header in
	Bytes.cat hdata mdata
;;