type 'a t = {
	q			        :	'a Queue.t;
	qlock		        :	Mutex.t;
	mutable qlast		:	float;
};;


let create () = {
    q= Queue.create ();
    qlock= Mutex.create ();
    qlast= Unix.time ();
};;

let clear q = 
	Mutex.lock q.qlock;
	Queue.clear q.q;
	q.qlast <- Unix.time ();
	Mutex.unlock q.qlock;
;;

let add q e =
	Mutex.lock q.qlock;
	Queue.add e q.q;
	q.qlast <- Unix.time ();
	Mutex.unlock q.qlock;
;;

let get q = 
	Mutex.lock q.qlock;
	let r = if Queue.is_empty q.q then None else Some (Queue.take q.q) in
	Mutex.unlock q.qlock;	
	r
;;

let getn q = None;;

let len q = Queue.length q.q;;