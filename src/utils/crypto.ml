open Cryptokit;;

let sha1 data = hash_string (Hash.sha1 ()) data;;
let sha256 data = hash_string (Hash.sha2 256) data;;
let ripemd160 data = hash_string (Hash.ripemd160 ()) data;;

let hash160 data = ripemd160 (sha256 data);;
let hash256 data = sha256 (sha256 data);;
let dsha256 data = hash256 data;;

let checksum4 data = String.sub (dsha256 data) 0 4;;