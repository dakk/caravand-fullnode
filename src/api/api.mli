(* REST api interface *)
open Blockchain
open Network


module Request : sig
    type m = GET | POST | PUT | DELETE

	type t = {
		uri		: string list;
		data	: string;
		rmethod	: m;
		socket 	: Unix.file_descr;
	}

	val recv    : Unix.file_descr -> t option
    val reply   : t -> int -> Yojson.Basic.json -> unit
end


val loop : int -> Chain.t -> Net.t -> unit
