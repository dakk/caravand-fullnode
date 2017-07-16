open Blockchain

type t

val init : Params.t -> int -> t
val loop : t -> Chain.t -> unit
