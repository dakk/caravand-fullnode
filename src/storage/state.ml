open Bitcoinml;;

module Utx_index = Store.Make_index 
  (struct 
    type t = Tx.Out.t;;

    let serialize tob = Tx.Out.serialize tob;;
    
    let parse data = 
      match Tx.Out.parse (Bitstring.bitstring_of_string data) with
		  | (rest, txo) -> txo
      | _ -> None
    ;;
  end) 
  (struct let prefix = "ut" end)
;;

let get_utx	state_store tx index = 
  Utx_index.get state_store @@ Hash.to_bin_norev tx ^ string_of_int index
;;

let insert_utx state_store txh index out =
  Utx_index.set state_store (Hash.to_bin_norev txh ^ string_of_int index) out
;;

let remove_utx	state_store tx index = 
  Utx_index.remove state_store @@ Hash.to_bin_norev tx ^ string_of_int index
;;