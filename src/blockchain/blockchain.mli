open Tx;;
open Block;;
open Params;;


module Blockchain : sig
	type t

	val genesis	: Params.t -> t
	val add		: t -> Block.t -> t
	val fork	: t -> t
	val height	: t -> int
	(*val last	: t -> Block.t
	val get		: t -> string -> Block.t
	val geti	: t -> int -> Block.t
	*)
	val utxo	: t -> Tx.t list
end


(*
module ChainSet : sig
	type t
	
	val add			: t -> Block.t -> t
	val genesis		: Params.t -> t

	val best_chain	: t -> Blockchain.t
	val drop_orphans: t -> t
end


module MemPool : sig
	type t = Tx.t list

	val add		: t -> Tx.t -> t
	val clean	: t -> Block.t -> t
end
*)
(* Le fork della blockchain sono diversi oggetti Blockchain.t, serve anche una mempool per ogni chain *)
