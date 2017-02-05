(* REST api interface *)
open Blockchain


module Request : sig
    type m = GET | POST

	type t = {
		uri		: string list;
		data	: string;
		rmethod	: m;
		socket 	: Unix.file_descr;
	}

	val recv    : Unix.file_descr -> t option
    val reply   : t -> int -> Yojson.Basic.json -> unit
end


val loop : int -> Blockchain.t -> unit

(*
    /transaction/:id
    Get a transaction

    /address/:id/balance
    Balance of an address

    /address/:id/utxo
    Utxo of an address

    /address/:id/txs
    Txs of an address

    /mempool/
    /mempool/stats
    Return the complete mempool or a stats

    /push
    Push a transaction in the mempool

    /block/:index
    /block/:hash
    Return a block given its index or hash

    / 
    Return the chainstate

    Needed indexes:
    - index -> block
    - hash -> block
    - id -> tx
    - address -> (utxo, balance, txs)
*)