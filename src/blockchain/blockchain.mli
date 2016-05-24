open Tx;;
open Block;;
open Params;;


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


(*
module ChainSet : sig
	type t
	
	val add			: t -> Block.t -> t
	val genesis		: Params.t -> t

	val best_chain	: t -> Blockchain.t
	val drop_orphans: t -> t
end
*)

(* Le fork della blockchain sono diversi oggetti Blockchain.t, serve anche una mempool per ogni chain *)
