open Conv;;

type 'a t = {
	q			        	:	'a Queue.t;
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

let can_add q = if (Obj.reachable_words (Obj.repr q.q)) < 50000000 then true else false;;

let add q e =
	if can_add q then (
		Mutex.lock q.qlock;
		Queue.add e q.q;
		q.qlast <- Unix.time ();
		Mutex.unlock q.qlock;
	) else (
		Printf.printf "dropping\n%!";
	)
;;

let (<<) q e = add q e;;

let get q = 
	Mutex.lock q.qlock;
	let r = if Queue.is_empty q.q then None else Some (Queue.take q.q) in
	Mutex.unlock q.qlock;	
	r
;;

let get_n q = None;;

let length q = Queue.length q.q;;

let rec iter q f = match get q with
| None -> 0
| Some (qel) -> f qel; 1 + (iter q f)
;;