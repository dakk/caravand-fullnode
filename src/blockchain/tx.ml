open Stdint;;
open Bitstring;;

let parse_varint bits =
	let parse_tag_byte bits =
		bitmatch bits with
		| { tag : 1*8 : string; rest : -1 : bitstring } -> (Uint8.of_bytes_little_endian tag 0, rest)
	in
	let parse_value bits bytesize =
		bitmatch bits with
		| { value : bytesize * 8 : string; rest : -1 : bitstring } -> 
			match bytesize with
			| 8 -> (Uint64.of_bytes_little_endian value 0, rest)
			| 4 -> (Uint32.to_uint64 (Uint32.of_bytes_little_endian value 0), rest)
			| 2 -> (Uint16.to_uint64 (Uint16.of_bytes_little_endian value 0), rest)
			| _ -> failwith "Varint parse error"
	in
	let tag, rest = parse_tag_byte bits in
		match Uint8.to_int tag with
		| 0xFF -> parse_value rest 8
		| 0xFE -> parse_value rest 4
		| 0xFD -> parse_value rest 2
		| x -> (Uint64.of_uint8 tag, rest)
;;


let bitstring_of_varint i = 
	match i with
	| i when i < 0xFDL -> BITSTRING { Int64.to_int i : 1*8 : littleendian }
	| i when i < 0xFFFFL -> BITSTRING { 0xFD : 1*8; Int64.to_int i : 2*8 : littleendian }
	| i when i < 0xFFFFFFFFL -> BITSTRING { 0xFE : 1*8; Int64.to_int32 i : 4*8 : littleendian }
	| i -> BITSTRING { 0xFF : 1*8; i : 8*8 : littleendian }
;;

module In = struct 
	type t = {
		out_hash: string;
		out_n	: uint32;
		script	: Script.t;
		sequence: uint32;		
	};;

	let serialize txin = "";;

	let parse data = 
		let bdata = bitstring_of_string data in
		bitmatch bdata with 
		| {
			out_hash	: 32*8: string; 
			out_n		: 32 : littleendian;
			rest		: -1 : bitstring
		} -> 
			let sclen, rest' = parse_varint rest in
			bitmatch rest' with
			| {
				script 		: Uint64.to_int (sclen) * 8 : string;
				sequence	: 32 : littleendian;
				rest'		: -1 : bitstring
			} -> (string_of_bitstring rest', Some {
				out_hash= Hash.of_bin out_hash;
				out_n= Uint32.of_int32 out_n;
				script= script;
				sequence= Uint32.of_int32 sequence;
			})
	;;

	let parse_all data = ("", []);;
end

module Out = struct 
	type t = {
		value	: int64;
		script	: Script.t;	
	};;

	let serialize txout = "";;
	
	let parse data =
		let bdata = bitstring_of_string data in
		bitmatch bdata with 
		| {
			value		: 64 : littleendian;
			rest		: -1 : bitstring
		} -> 
			let sclen, rest' = parse_varint rest in
			bitmatch rest' with
			| {
				script 		: Uint64.to_int (sclen) * 8 : string;
				rest''		: -1 : bitstring
			} -> (string_of_bitstring rest'', Some { value= value; script= script; })
	;;

	let parse_all data = ("", []);;
end


type t = {
	hash		: Hash.t;
	version		: int32;
	txin 		: In.t list;
	txout 		: Out.t list;
	locktime	: uint32;
};;


let parse data = 
	let bdata = bitstring_of_string data in
	bitmatch bdata with 
	| {
		version		: 32 : littleendian;
		rest		: -1 : bitstring
	} -> 
		let rest', txin = In.parse_all (string_of_bitstring rest) in
		let rest', txout = Out.parse_all (rest') in
		let bdata = bitstring_of_string rest' in
		bitmatch bdata with 
		| {
			locktime	: 32 : littleendian;
			rest		: -1 : bitstring
		} -> (string_of_bitstring rest, Some {
			hash	= Hash.of_bin (Crypto.hash256 data);
			version	= version;
			txin	= txin;
			txout	= txout;
			locktime= Uint32.of_int32 locktime;
		})
;;


let serialize tx = "";;