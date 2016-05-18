open Unsigned;;

module Stack : sig
	type item = Unsigned.U32;;
	
	type t 
	
	val stack_top	:	t -> item
	val stack_push	:	t -> item -> t
	val stack_pop	:	t -> item * t
	val stack_size	:	t -> int
	val stack_empty :	unit -> t
end


module Vm : sig
	
	
	val vm_init 	:	unit -> Stack.t

end