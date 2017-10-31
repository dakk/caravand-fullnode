open Stdint;;
open Bitcoinml;;
open Block;;
open Header;;

type t = {
  (* Fork status *)
	fork_hash	    :	Hash.t;
  fork_height	  :	int64;

	(* Last header status *)
	mutable header_height	:	int64;
  mutable header_last		: Block.Header.t;

  mutable header_list   : Block.Header.t list;
};;


let create fhash fheight header = {
  fork_hash= fhash;
  fork_height= fheight;
  header_height= Int64.succ fheight;
  header_last= header;
  header_list= [header];
};;

let last b = b.header_last.hash;;

let push b head =
  b.header_height <- Int64.succ b.header_height;
  b.header_last <- head;
  b.header_list <- b.header_list @ [head];
  true
;;

let parse data = 
  let rec parse_headers rest n hlist = match n with
  | 0 -> (rest, Some (hlist))
  | n ->
    let chead = String.sub rest 0 80 in
    let rest' = String.sub rest 80 @@ (String.length rest) - 80 in
    match Block.Header.parse chead with
    | None -> (rest', None)
    | Some (h) ->
      parse_headers rest' (n-1) @@ hlist @ [h]
  in
	let bdata = Bitstring.bitstring_of_string data in
	match%bitstring bdata with
  | {|
    fork_hash  : 32*8 : string;
    fork_height: 64 : littleendian;
    header_height: 64 : littleendian;
    header_n : 32 : littleendian;
    header_last : 80 : string;
    rest : -1 : bitstring
  |} -> 
    match parse_headers (Bitstring.string_of_bitstring rest) (Int32.to_int header_n) [] with
    | (rest', None) -> (rest', None)
    | (rest', Some (hlist)) ->
      match Block.Header.parse header_last with
      | Some (h) -> (rest', Some {
        fork_hash= Hash.of_bin fork_hash;
        fork_height= fork_height;
        header_height= header_height;
        header_last= h;
        header_list= hlist;
      })
      | None -> (rest', None)
;;

let serialize b =
  Bitstring.string_of_bitstring [%bitstring {| 
    Hash.to_bin b.fork_hash   : 32*8 : string;
    b.fork_height			      	: 64 : littleendian;
    b.header_height			      : 64 : littleendian;
    Int32.of_int @@ List.length b.header_list : 32 : littleendian |}] ^
  Block.Header.serialize b.header_last ^
  List.fold_left (^) "" @@ List.map (fun h -> Block.Header.serialize h) b.header_list
;;

let rec find_parent branches header = 
  try Some (List.find (fun b -> (header.prev_block) = last b) branches)
  with | Not_found -> None
;;

let rec find_fork branches header =
  try Some (List.find (fun b -> (header.prev_block) = b.fork_hash) branches)
  with | Not_found -> None
;;

let best_branch branches = 
  let rec bc' bl b = match (bl, b) with
  | ([], b) -> b
  | (b' :: bl', None) -> bc' bl' @@ Some (b')
  | (b' :: bl', Some (b)) -> if b'.header_height > b.header_height then bc' bl' @@ Some (b') else (bc' bl' @@ Some (b))
  in bc' branches None
;;