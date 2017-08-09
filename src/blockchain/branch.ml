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
  header_height= Int64.add fheight Int64.one;
  header_last= header;
  header_list= [header];
};;

let last b = b.header_last.hash;;

let push b head =
  b.header_height <- Int64.add Int64.one b.header_height;
  b.header_last <- head;
  b.header_list <- b.header_list @ [head];
  true
;;

let parse data = 
  let rec parse_headers rest n hlist = match n with
  | 0 -> (rest, Some (hlist))
  | _ -> 
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
    header_last : 80 : string;
    header_n : 32 : littleendian;
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
  let headers =
    List.fold_left (^) "" @@ List.map (fun h -> Block.Header.serialize h) b.header_list
  in
	[%bitstring {|
		Hash.to_bin b.fork_hash   : 32*8 : string;
		b.fork_height			      	: 64 : littleendian;
		b.header_height			      : 64 : littleendian;
    Block.Header.serialize b.header_last   : 80 : string;
    Int32.of_int @@ List.length b.header_list : 32 : littleendian;
    headers                   : 80*(List.length b.header_list) : string
  |}] |> Bitstring.string_of_bitstring
;;



let rec find_parent branches header = match branches with
| [] -> None
| b :: bl -> if (header.prev_block) = last b then Some (b) else find_parent bl header
;;
