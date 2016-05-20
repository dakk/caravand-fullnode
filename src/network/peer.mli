open Message

type t

val connect : Unix.inet_addr -> int -> t option
val send	: t -> Message.t -> unit
val recv	: t -> Message.t option