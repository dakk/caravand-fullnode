type t = bytes;;
type hash = t;;

let rec to_string h =
	let tos c = Printf.sprintf "%02x" (int_of_char c) in
	match String.length h with
	| 0 -> ""
	| n -> (tos h.[0]) ^ to_string (String.sub h 1 ((String.length h) - 1))
;;

let from_string	s = s;;