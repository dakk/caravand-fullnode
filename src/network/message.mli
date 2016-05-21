open Params;;

type header

type version
(*type ping
type pong*)

type t

(* Parse the header from the given 24 bytes *)
val parse_header: bytes -> header

(* Parse the payload *)
val parse		: header -> bytes -> t

(* Serialize the message (the result include also the header) *)
val serialize	: Params.t -> t -> bytes