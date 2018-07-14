open Blockchain
open Utils
open Bitcoinml

type t

val init : Params.t -> Config.t -> t
val loop : t -> Chain.t -> unit
val step : t -> Chain.t -> unit
val peer_of_addr : t -> Unix.inet_addr -> Peer.t option
val connected_peers : t -> int
val sent : t -> int
val received : t -> int
val shutdown : t -> unit