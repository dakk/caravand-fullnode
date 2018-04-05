open Utils;;
open LevelDB;;
open Bitcoinml;;
open Stdint;;
open Store;;

module Address = struct
	type t = {
		mutable balance		: int64;
		mutable sent			: int64;
		mutable received	: int64;
		mutable txs				: int64;
		mutable utxs			: int64;
	};;

	type utx = string * int * int64;;

	let parse data = 
		let bdata = Bitstring.bitstring_of_string data in
		match%bitstring bdata with
		| {|
			balance		: 64 	: littleendian;
			sent			: 64 	: littleendian;
			received	: 64	: littleendian;
			txs				: 64	: littleendian;
			utxs			: 64	: littleendian
		|} ->
		Some ({
			balance		= balance;
			sent		= sent;
			received	= received;
			txs			= txs;
			utxs		= utxs;
		})
	;;

	let serialize addr = 
		let bs = [%bitstring {|
			addr.balance	: 64 : littleendian;
			addr.sent	  	: 64 : littleendian;
			addr.received	: 64 : littleendian;
			addr.txs	  	: 64 : littleendian;
			addr.utxs	  	: 64 : littleendian
		|}] in Bitstring.string_of_bitstring bs
  ;;
end


module Address_index = Store.Make_index 
  (Address)
  (struct let prefix = "ad" end)
;;

module Address_tx_index = Store.Make_index 
  (struct 
		type t = string;;
		
    let serialize txhash = Bitstring.string_of_bitstring ([%bitstring {|
			Hash.to_bin (txhash)	: 32*8 : string
		|}]);;

		let parse o = 
			match%bitstring Bitstring.bitstring_of_string o with
			|  {| txhash		: 32*8 	: string |} -> 
				Some (Hash.of_bin txhash)
		;;
  end)
  (struct let prefix = "at" end)
;;

module Address_utx_index = Store.Make_index 
  (struct 
		type t = (Hash.t * int * Int64.t);;
		
		let serialize o = 
			match o with
			| (txhash, i, value) -> Bitstring.string_of_bitstring ([%bitstring {|
				Hash.to_bin (txhash)	: 32*8 : string;
				Int32.of_int i			: 32 : littleendian;
				value					: 64 : littleendian
			|}])
		;;

		let parse o = 
			match%bitstring Bitstring.bitstring_of_string o with
			|  {|
				txhash	: 32*8 	: string;
				i				: 32 	: littleendian;
				value		: 64	: littleendian
			|} -> 
				Some (Hash.of_bin txhash, Int32.to_int i, value)
		;;
  end)
  (struct let prefix = "au" end)
;;

let remove_tx adr_store addr txhash time =
	Address_tx_index.remove adr_store (addr ^ string_of_float time ^ Hash.to_bin_norev txhash)
;;

let remove_utxo adr_store addr txhash i =
	Address_utx_index.remove adr_store (addr ^ Hash.to_bin_norev txhash ^ string_of_int i)
;;

let add_tx adr_store addr txhash time =
	Address_tx_index.set adr_store (addr ^ string_of_float time ^ Hash.to_bin_norev txhash) txhash
;;

let add_utxo adr_store addr txhash i value =
	Address_utx_index.set adr_store (addr ^ Hash.to_bin_norev txhash ^ string_of_int i) (txhash, i, value)
;;


let get_txs adr_store addr txs =
	let rec get_tx it n acc = match n with
	| 0 -> acc
	| n' -> 
		let h = Address_tx_index.get adr_store @@ LevelDB.Iterator.get_value it in
		let _ = LevelDB.Iterator.next it in
		get_tx it (n' - 1) @@ h :: acc
	in
	let it = Address_tx_index.iterator adr_store addr in
	get_tx it txs []
;;

let get_utxos adr_store addr utxs =
	let rec get_utx it n acc = match n with
	| 0 -> acc
	| n' -> 
		let ut = Address_utx_index.get adr_store @@ LevelDB.Iterator.get_value it in
		let _ = LevelDB.Iterator.next it in
		get_utx it (n' - 1) @@ ut :: acc
	in
	let it = Address_utx_index.iterator adr_store addr in
	get_utx it utxs []
;;

let save adr_store addr data = Address_index.set adr_store addr data;;

let load adr_store addr =
	let empty: Address.t = {
		balance		= Int64.zero;
		sent		= Int64.zero;
		received	= Int64.zero;
		txs			= Int64.zero;
		utxs		= Int64.zero;
	} in
	match Address_index.get adr_store addr with
	| Some (adr) -> adr
	| None -> empty
;;



module Address_cache = struct
	type t = (string, Address.t) Hashtbl.t;;

	let create () = Hashtbl.create 16;;

	let load address_tbl adr_store address =
		match Address_index.get adr_store address with
		| None -> 
			let addr = load adr_store address in
			Hashtbl.add address_tbl address addr;
			addr
		| Some (adr) -> adr
	;;

	let save address_tbl address addr =
		Hashtbl.replace address_tbl address addr
	;;

	let sync address_tbl adr_store =
		Hashtbl.iter (fun address addr -> 
			Address_index.set adr_store address addr
		) address_tbl
	;;
end