(* REST api interface *)
open Blockchain
open Network
open Utils


module Request : sig
    type m = GET | POST | PUT | DELETE

	type t = {
		uri		: string list;
		data	: Yojson.Basic.json option;
		rmethod	: m;
		socket 	: Unix.file_descr;
	}

	val recv    : Unix.file_descr -> t option
end

type t

val init 		: Config.rest -> Chain.t -> Net.t -> t
val loop 		: t -> unit
val shutdown 	: t -> unit
