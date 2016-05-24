type t = bytes;;
type hash = t;;

let reverse s =
  let rec reverse_string_acc s acc index length =
    if index >= length
    then acc
    else reverse_string_acc s ((String.make 1 s.[index]) ^ acc) (index+1) length
  in
  reverse_string_acc s "" 0 (String.length s)
;;


let to_string h' =
	let rec to_string' h =
		let tos c = Printf.sprintf "%02x" (int_of_char c) in
		match String.length h with
		| 0 -> ""
		| n -> (tos h.[0]) ^ to_string' (String.sub h 1 ((String.length h) - 1))
	in 
	let ss = to_string' (reverse h') in ss
	(*
	in "000000" ^ (String.sub (ss) 0 (64 - 6))*)
;;

let bhash_to_string h' =
	let h = to_string h' in "000000" ^ (String.sub (h) 0 (64 - 6))
;;


let from_string	s = s;;