open Cryptokit;;

let sha256 data =
  hash_string (Hash.sha256 ()) data
;;

let checksum4 data =
	String.sub (sha256 data) 0 4
;;