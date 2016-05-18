open Unsigned;;

module Script : sig
	type op
	type t = Unsigned.U8 list

	val op_to_string    : op -> string
	val op_to_hex       : op -> Unsigned.U8	
end