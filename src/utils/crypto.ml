open Cryptokit;;

let sha1 data = hash_string (Hash.sha1 ()) data;;
let sha256 data = hash_string (Hash.sha256 ()) data;;

let ripemd160 data = hash_string (Hash.ripemd160 ()) data;;
let dsha256 data = sha256 (sha256 data);;

let hash160 data = ripemd160 (sha256 data);;
let hash256 data = dsha256 data;;

let checksum4 data = String.sub (dsha256 data) 0 4;;