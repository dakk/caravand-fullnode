open Unix;;

(* Query a DNS server for node addresses *)
val query 		: string -> inet_addr list

(* Query a set of DNS servers for node addresses *)
val query_set 	: string list -> inet_addr list
