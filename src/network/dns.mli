open Core.Std;;
open Async.Std;;

module Dns : sig
	val query : 
		server 	: string -> 
		unit
end
