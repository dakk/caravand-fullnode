open Stdint;;
open Bitstring;;
open Parser;;

module In = struct 
	type t = {
		out_hash: string;
		out_n	: uint32;
		script	: Script.t;
		sequence: uint32;		
	};;

	let serialize txin = 
		let out = Bitstring.string_of_bitstring (BITSTRING { 
			Hash.to_bin txin.out_hash : 32 * 8 : string;
			Uint32.to_int32 txin.out_n : 32 : littleendian
		}) in 
		let sclen = string_of_bitstring (Parser.bitstring_of_varint (Int64.of_int (Script.length txin.script))) in
		let sequence = Bitstring.string_of_bitstring (BITSTRING { Uint32.to_int32 txin.sequence : 32 : littleendian }) in 
		out ^ sclen ^ Script.serialize (txin.script) ^ sequence
	;;

	let serialize_all txins = 
		let rec serialize_all' ins = match ins with
		| [] -> ""
		| i::ins' -> (serialize i) ^ (serialize_all' ins')
		in
		let len = string_of_bitstring (Parser.bitstring_of_varint (Int64.of_int (List.length txins))) in
		len ^ (serialize_all' txins) 
	;;

	let parse bdata = 
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
			} -> 
				let sc = Script.parse script in
				Printf.printf "ScriptIn: %s\n%!" (Script.to_string sc);		
				(rest', Some {
					out_hash= Hash.of_bin out_hash;
					out_n= Uint32.of_int32 out_n;
					script= sc;
					sequence= Uint32.of_int32 sequence;
				})
	;;

	let parse_all data = 
		let inlen, rest' = parse_varint data in
		let rec parse_all' n d acc = match n with
		| 0 -> (d, acc)
		| n -> 
			let rest, txin = parse d in
			match txin with
			| None -> parse_all' (n-1) rest acc
			| Some (txi) -> parse_all' (n-1) rest (txi::acc)
		in parse_all' (Uint64.to_int inlen) rest' []
	;;
end

module Out = struct 
	type t = {
		value	: int64;
		script	: Script.t;	
	};;

	let serialize txout = 
		let value = Bitstring.string_of_bitstring (BITSTRING { txout.value : 64 : littleendian }) in 
		let sclen = string_of_bitstring @@ Parser.bitstring_of_varint (Int64.of_int (Script.length txout.script)) in
		value ^ sclen ^ Script.serialize (txout.script)
	;;

	let serialize_all txouts = 
		let rec serialize_all' outs = match outs with
		| [] -> ""
		| o::outs' -> (serialize o) ^ (serialize_all' outs')
		in
		let len = string_of_bitstring @@ Parser.bitstring_of_varint (Int64.of_int (List.length txouts)) in
		len ^ (serialize_all' txouts) 
	;;
	
	let parse bdata =
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
			} -> 
			let sc = Script.parse script in
			Printf.printf "ScriptOut: %s\n%!" (Script.to_string sc);			
			(rest'', Some { value= value; script= sc; })
	;;


	let parse_all data = 
		let outlen, rest' = parse_varint data in
		let rec parse_all' n d acc = match n with
		| 0 -> (d, acc)
		| n -> 
			let rest, txout = parse d in
			match txout with
			| None -> parse_all' (n-1) rest acc
			| Some (txo) -> parse_all' (n-1) rest (txo::acc)
		in parse_all' (Uint64.to_int outlen) rest' []
	;;
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
		let rest', txin = In.parse_all rest in
		let rest'', txout = Out.parse_all rest' in
		let bdata = rest'' in
		bitmatch bdata with 
		| {
			locktime	: 32 : littleendian;
			rest		: -1 : bitstring
		} -> 
			let rest''' = string_of_bitstring rest in
			let txlen = (Bytes.length data) - (Bytes.length rest''') in
			let txhash = Hash.of_bin (Crypto.hash256 (Bytes.sub data 0 txlen)) in
			(rest''', Some {
				hash	= txhash;
				version	= version;
				txin	= List.rev txin;
				txout	= List.rev txout;
				locktime= Uint32.of_int32 locktime;
			})
;;


let parse_all data n =
	let rec parse_all' n d acc = match n with
	| 0 -> acc
	| n ->
		let rest, tx = parse d in
		match tx with
		| None -> parse_all' (n-1) rest acc
		| Some (mtx) -> parse_all' (n-1) rest (mtx::acc)
	in
	parse_all' n data []
;;

let print tx = 
	Printf.printf ""; ()
;;

let serialize tx = 
	let res = Bitstring.string_of_bitstring (BITSTRING { tx.version : 32 : littleendian }) in 
	let res = res ^ (In.serialize_all tx.txin) ^ (Out.serialize_all tx.txout) in
	let ltime = Bitstring.string_of_bitstring (BITSTRING { Uint32.to_int32 tx.locktime : 32 : littleendian }) in 
	res ^ ltime
;;

let rec serialize_all txs = match txs with
| [] -> ""
| tx::txs' -> (serialize tx) ^ (serialize_all txs')
;;