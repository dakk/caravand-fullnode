open Bitcoinml;;
open Blockchain;;
open Stdint;;
open OUnit2;;
open Hex;;
open Letchain;;


let branch_serialize_test rawh octx =
	let bl = Block.Header.parse (Hex.to_string rawh) in
	match bl with
	| None -> assert_equal true false
	| Some (b) -> 
    let br = Branch.create b.hash (Int64.of_int 123) b in
		let brb = Branch.serialize br in
		let _ = Branch.parse brb in
    assert_equal true true
;;

let letchain_init () octx = 
	let lc = Letchain.init ~loglevel:0 (PrunedNode (1024)) in
	assert_equal false @@ Letchain.is_synchronized lc
;;

let suite = "letchain" >::: [
	"letchain.init" >:: letchain_init ();
	"branch.serialize_and_parse" >:: branch_serialize_test (`Hex "02000000f6e1cc50df9bfb420162e365fd26d783581367c0a4a7f2683ee60702000000000e65cda8974f3989caeafcaa46ad665ffd07fe558cb63f3f639fee284db83aa4436c6b500045011cec2b25fb");
];;

let () = run_test_tt_main suite;;