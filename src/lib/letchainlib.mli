type node_type = 
| FullNode            (* Node with full block data*)
| PrunedNode of int   (* Node with full block data of last n blocks (Address disabled) *)
| HeadersOnly         (* Node with only headers *)

type t

val init : ?directory:string -> ?network:string -> ?peers:int -> node_type -> t
val run  : t -> Thread.t
val kill : t -> bool
val stop : t -> bool

val is_sync : t -> bool