open Blockchain
open Utils

type t

val init : Params.t -> Config.t -> t
val loop : t -> Chain.t -> unit
