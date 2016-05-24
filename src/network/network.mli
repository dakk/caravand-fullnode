open Params;;

type t

val init : Params.t -> t
val loop : t -> Blockchain.t -> unit
