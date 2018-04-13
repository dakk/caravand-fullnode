open Yojson
open Unix

val send : Unix.file_descr -> string -> unit
val recv : Unix.file_descr -> string
val shutdown : Unix.file_descr -> unit
val listen : Unix.file_descr -> int -> int -> unit

module HTTP : sig
  type met = GET | POST | DELETE | PUT
  type req = {
		uri		: string list;
    body	: string;
    body_json : Yojson.Basic.json option;
		rmethod	: met;
		socket 	: Unix.file_descr;
  }

  val reply_json : req -> int -> Yojson.Basic.json -> unit
  val reply : req -> int -> string -> unit
  val parse_request : Unix.file_descr -> req option
end

module JSONRPC : sig
  type req = {
    methodn : string;
    params  : Yojson.Basic.json list;
    id      : string;
		socket 	: Unix.file_descr;
  }

  val parse_request : Unix.file_descr -> req option
  val reply : req -> Yojson.Basic.json -> unit
end