open Bitcoinml;;

type t = {
  storage_prefix        : string;

  (* Fork status *)
	fork_hash	    :	Hash.t;
  fork_height	  :	int64;

	(* Last header status *)
	mutable header_height	:	int64;
  mutable header_last		: Block.Header.t;

  mutable header_list   : Block.Header.t list;
};;


let create fhash fheight header = {
  storage_prefix= "br" ^ String.sub (Hash.to_string fhash) 0 4;
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