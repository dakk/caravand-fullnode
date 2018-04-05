open Utils;;
open LevelDB;;
open Bitcoinml;;
open Stdint;;
open Store;;

module Block_index = Store.Make_index 
  (Block)
  (struct let prefix = "bk" end)
;;

module Block_header_index = Store.Make_index 
  (Block.Header)
  (struct let prefix = "bk" end)
;;

module Block_height_index = Store.Make_index 
  (struct 
    type t = string;;
    let serialize o = o;;
    let parse o = Some o;;
  end) 
  (struct let prefix = "bi" end)
;;

module Block_height_index_reverse = Store.Make_index 
  (struct 
    type t = Uint32.t;;
    let serialize o = Printf.sprintf "%d" @@ Uint32.to_int o;;
    let parse o = Some (Uint32.of_int @@ int_of_string o);;
  end) 
  (struct let prefix = "bh" end)
;;

let test_bi () =
  let blocks_store = Store_raw.load "test" "blocks" in
  Block_index.remove blocks_store "ciao";
  Block_index.sync blocks_store
;;