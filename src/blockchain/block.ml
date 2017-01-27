open Stdint;;
open Bitstring;;
open Crypto;;

module Header = struct
	type t = {
		hash		: Hash.t;
		version		: int32;
		prev_block	: Hash.t;
		merkle_root : Hash.t;
		time		: float;
		bits		: uint32;
		nonce		: uint32;	
	};;

	let serialize h = 
		let btime = Bytes.create 4 in
		Uint32.to_bytes_little_endian (Uint32.of_float h.time) btime 0;
		let bbits = Bytes.create 4 in
		Uint32.to_bytes_little_endian h.bits bbits 0;
		let bnonce = Bytes.create 4 in
		Uint32.to_bytes_little_endian h.nonce bnonce 0;
		let bs = BITSTRING {
			h.version 							: 4*8 : littleendian;
			Hash.to_bin h.prev_block			: 32*8: string; 
			Hash.to_bin h.merkle_root			: 32*8: string;
			btime								: 32 : string;
			bbits								: 32 : string;
			bnonce								: 32 : string
		} in Bitstring.string_of_bitstring bs
	;;
	
	let parse data = 
		let check_target h b =
		(*	let bigint_of_hash h = 
				let res7 = Big_int.big_int_of_int64 (Int64.of_string ("0x" ^ (String.sub h 0 8))) in
				let result = Big_int.shift_left_big_int res7 (7 * 32) in
				let res6 = Big_int.big_int_of_int64 (Int64.of_string ("0x" ^ (String.sub h 8 8))) in
				let result = Big_int.or_big_int result (Big_int.shift_left_big_int res6 (6 * 32)) in
				let res5 = Big_int.big_int_of_int64 (Int64.of_string ("0x" ^ (String.sub h 16 8))) in
				let result = Big_int.or_big_int result (Big_int.shift_left_big_int res5 (5 * 32)) in
				let res4 = Big_int.big_int_of_int64 (Int64.of_string ("0x" ^ (String.sub h 24 8))) in
				let result = Big_int.or_big_int result (Big_int.shift_left_big_int res4 (4 * 32)) in
				let res3 = Big_int.big_int_of_int64 (Int64.of_string ("0x" ^ (String.sub h 32 8))) in
				let result = Big_int.or_big_int result (Big_int.shift_left_big_int res3 (3 * 32)) in
				let res2 = Big_int.big_int_of_int64 (Int64.of_string ("0x" ^ (String.sub h 40 8))) in
				let result = Big_int.or_big_int result (Big_int.shift_left_big_int res2 (2 * 32)) in
				let res1 = Big_int.big_int_of_int64 (Int64.of_string ("0x" ^ (String.sub h 48 8))) in
				let result = Big_int.or_big_int result (Big_int.shift_left_big_int res1 (1 * 32)) in
				let res0 = Big_int.big_int_of_int64 (Int64.of_string ("0x" ^ (String.sub h 56 8))) in
				Big_int.or_big_int result (Big_int.shift_left_big_int res0 (0 * 32))
			in*)
		(*	let calc_target b = 
				let exp = Bytes.sub b 0 2 in
				let body = Bytes.sub b 2 6 in
				Big_int.zero
			in
			let t' = calc_target b
			let h' = Big_int.of_string h
			Big_int.lt_big_int h' t'			
		*)
			true
		in
		let bdata = bitstring_of_string data in
		bitmatch bdata with 
		| {
			version 	: 4*8 : littleendian;
			prev_block	: 32*8: string; 
			merkle_root	: 32*8: string;
			time		: 32 : string;
			bits		: 32 : string;
			nonce		: 32 : string
		} ->
		let hash = Hash.of_bin (hash256 data) in
		if check_target hash bits then
			Some {
				hash			= hash;
				version			= version;
				prev_block		= Hash.of_bin prev_block;
				merkle_root		= Hash.of_bin merkle_root;
				time			= Uint32.to_float (Uint32.of_bytes_little_endian time 0);
				bits			= Uint32.of_bytes_little_endian bits 0;
				nonce			= Uint32.of_bytes_little_endian nonce 0;
			}
		else
			None
	;;
end

type t = {
	header	: Header.t;
	txs		: Tx.t list;
};;


let parse data =
	let h = Header.parse data in
	match h with 
	| None -> None
	| Some (header) -> Some {
		header= header;
		txs= [];
	}
;;



let serialize block = "";