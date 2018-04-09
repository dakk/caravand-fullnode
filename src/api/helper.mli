open Yojson
open Unix

val send : Unix.file_descr -> string -> unit
val recv : Unix.file_descr -> string

module HTTP : sig
  type met = GET | POST | DELETE | PUT
  type req = {
		uri		: string list;
    body	: string;
    body_json : Yojson.Basic.json option;
		rmethod	: met;
		socket 	: Unix.file_descr;
  }

  val reply_json : Unix.file_descr -> int -> Yojson.Basic.json -> unit
  val reply : Unix.file_descr -> int -> string -> unit
  val parse_request : Unix.file_descr -> req option
end