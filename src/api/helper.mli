open Yojson
open Unix

val send : Unix.file_descr -> string -> unit
val recv : Unix.file_descr -> string

module HTTP : sig
  val send_json : Unix.file_descr -> int -> Yojson.Basic.json -> unit
end