type 'a t = {
	q								:	'a Queue.t;
	qlock		        :	Mutex.t;
	mutable qlast		:	float;
}

val create  : unit -> 'a t
val add     : 'a t -> 'a -> unit
val (<<)		: 'a t -> 'a -> unit
val get     : 'a t -> 'a option
val get_n   : 'a t -> 'a list option
val length	: 'a t -> int
val clear		: 'a t -> unit
val iter		: 'a t -> ('a -> unit) -> int
