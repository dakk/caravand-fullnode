type 'a t = {
	q			        :	'a Queue.t;
	qlock		        :	Mutex.t;
	mutable qlast		:	float;
}

val create  : unit -> 'a t
val add     : 'a t -> 'a -> unit
val get     : 'a t -> 'a option
val getn    : 'a t -> 'a list option
val len     : 'a t -> int
val clear	: 'a t -> unit
