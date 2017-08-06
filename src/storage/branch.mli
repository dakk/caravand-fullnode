type t

val get_height : t -> Int64.t
val get_hash   : t -> Hash.t

val delete     : t -> bool
val create     : unit -> t