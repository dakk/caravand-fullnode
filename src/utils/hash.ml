type t = string;;
type hash = t;;
type b = bytes;;


(* Helper *)
let reverse s =
	let rec reverse_string_acc s acc index length =
		if index >= length
		then acc
		else reverse_string_acc s ((String.make 1 s.[index]) ^ acc) (index+1) length
	in
	reverse_string_acc s "" 0 (String.length s)
;;


(* Binary to hash *)
let of_bin b =
	let rec of_bin' h =
		let tos c = Printf.sprintf "%02x" (int_of_char c) in
		match String.length h with
		| 0 -> ""
		| n -> (tos h.[0]) ^ of_bin' (String.sub h 1 ((String.length h) - 1))
	in 
	of_bin' (reverse b)
;;

let of_binblock b =
	let h = of_bin b in "000000" ^ (String.sub (h) 0 (64 - 6))
;;

let zero () = String.make 64 '0';;


(* Hash to binary *)
let to_bin b = ""
;;

let to_binblock b = ""
;;

