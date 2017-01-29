open Params;;

type t

val init : Params.t -> int -> t
val loop : t -> Blockchain.t -> unit
