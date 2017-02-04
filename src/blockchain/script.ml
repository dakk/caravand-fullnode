module SStack = struct
    type t = bytes Stack.t;;

    let create () = Stack.create ();;

    let pop st = "";;
    let popi st = 0;;
    
    let top st = "";;
    let topi st = 0;;

    let push v st = ();;
    let pushi vi st = ();;
end


type opcode = 
(* Constants *)
| OP_0
| OP_FALSE
| OP_DATA of int * bytes
| OP_PUSHDATA1 of int *  bytes
| OP_PUSHDATA2 of int * int * bytes
| OP_PUSHDATA4 of int * int * int * int * bytes
| OP_1NEGATE
| OP_1
| OP_TRUE
| OP_2
| OP_3
| OP_4
| OP_5
| OP_6
| OP_7
| OP_8
| OP_9
| OP_10
| OP_11
| OP_12
| OP_13
| OP_14
| OP_15
| OP_16

(* Flow *)
| OP_IF
| OP_NOTIF
| OP_ELSE
| OP_ENDIF
| OP_VERIFY
| OP_RETURN

(* Stack *)
| OP_TOALTSTACK
| OP_FROMALTSTACK
| OP_IFDUP
| OP_DEPTH
| OP_DROP
| OP_DUP
| OP_NIP
| OP_OVER
| OP_PICK
| OP_ROLL
| OP_ROT
| OP_SWAP
| OP_TUCK
| OP_2DROP
| OP_2DUP
| OP_3DUP
| OP_2OVER
| OP_2ROT
| OP_2SWAP

(* Splice *)
| OP_CAT
| OP_SUBSTR
| OP_LEFT
| OP_RIGHT
| OP_SIZE

(* Bitwise logic *)
| OP_INVERT
| OP_AND
| OP_OR
| OP_XOR
| OP_EQUAL
| OP_EQUALVERIFY

(* Arithmetic*)
| OP_1ADD
| OP_1SUB
| OP_2MUL
| OP_2DIV
| OP_NEGATE
| OP_ABS
| OP_NOT
| OP_0NOTEQUAL
| OP_ADD
| OP_SUB
| OP_MUL
| OP_DIV
| OP_MOD
| OP_LSHIFT
| OP_RSHIFT
| OP_BOOLAND
| OP_BOOLOR
| OP_NUMEQUAL
| OP_NUMEQUALVERIFY
| OP_NUMNOTEQUAL
| OP_LESSTHAN
| OP_GREATERTHAN
| OP_LESSTHANOREQUAL
| OP_GREATERTHANOREQUAL
| OP_MIN
| OP_MAX
| OP_WITHIN

(* Crypto *)
| OP_RIPEMD160
| OP_SHA1
| OP_SHA256
| OP_HASH160
| OP_HASH256
| OP_CODESEPARATOR
| OP_CHECKSIG
| OP_CHECKSIGVERIFY
| OP_CHECKMULTISIG
| OP_CHECKMULTISIGVERIFY

(* Lock time *)
| OP_CHECKLOCKTIMEVERIFY
| OP_CHECKSEQUENCEVERIFY

(* Pseudo words *)
| OP_PUBKEYHASH
| OP_PUBKEY
| OP_INVALIDOPCODE

(* Reserved words*)
| OP_RESERVED
| OP_VER
| OP_VERIF
| OP_VERNOTIF
| OP_RESERVED1
| OP_RESERVED2
| OP_NOP of int
;;

type t = opcode list * int;;


let opcode_to_string oc = match oc with
| OP_0 -> "OP_0"
| OP_FALSE -> "OP_FALSE"
| OP_DATA (s, data) -> Printf.sprintf "OP_DATA(%x, %s)" s (Hash.of_bin data)
| OP_PUSHDATA1 (s, data) -> Printf.sprintf "OP_PUSHDATA1(%x, %s)" s (Hash.of_bin data)
| OP_PUSHDATA2 (s1, s2, data) -> Printf.sprintf "OP_PUSHDATA2(%x%x, %s)" s1 s2 (Hash.of_bin data)
| OP_PUSHDATA4 (s1, s2, s3, s4, data) -> Printf.sprintf "OP_PUSHDATA4(%x%x%x%x, %s)" s1 s2 s3 s4 (Hash.of_bin data)
| OP_1NEGATE -> "OP_1NEGATE"
| OP_1 -> "OP_1"
| OP_TRUE -> "OP_TRUE"
| OP_2 -> "OP_2"
| OP_3 -> "OP_3"
| OP_4 -> "OP_4"
| OP_5 -> "OP_5"
| OP_6 -> "OP_6"
| OP_7 -> "OP_7"
| OP_8 -> "OP_8"
| OP_9 -> "OP_9"
| OP_10 -> "OP_10"
| OP_11 -> "OP_11"
| OP_12 -> "OP_12"
| OP_13 -> "OP_13"
| OP_14 -> "OP_14"
| OP_15 -> "OP_15"
| OP_16 -> "OP_16"

(* Flow *)
| OP_IF -> "OP_IF"
| OP_NOTIF -> "OP_NOTIF"
| OP_ELSE -> "OP_ELSE"
| OP_ENDIF -> "OP_ENDIF"
| OP_VERIFY -> "OP_VERIFY"
| OP_RETURN -> "OP_RETURN"

(* Stack *)
| OP_TOALTSTACK -> "OP_TOALTSTACK"
| OP_FROMALTSTACK -> "OP_FROMALTSTACK"
| OP_IFDUP -> "OP_IFDUP"
| OP_DEPTH -> "OP_DEPTH"
| OP_DROP -> "OP_DROP"
| OP_DUP -> "OP_DUP"
| OP_NIP -> "OP_NIP"
| OP_OVER -> "OP_OVER"
| OP_PICK -> "OP_PICK"
| OP_ROLL -> "OP_ROLL"
| OP_ROT -> "OP_ROT"
| OP_SWAP -> "OP_SWAP"
| OP_TUCK -> "OP_TUCK"
| OP_2DROP -> "OP_2DROP"
| OP_2DUP -> "OP_2DUP"
| OP_3DUP -> "OP_3DUP"
| OP_2OVER -> "OP_2OVER"
| OP_2ROT -> "OP_2ROT"
| OP_2SWAP -> "OP_2SWAP"

(* Splice *)
| OP_CAT -> "OP_CAT"
| OP_SUBSTR -> "OP_SUBSTR"
| OP_LEFT -> "OP_LEFT"
| OP_RIGHT -> "OP_RIGHT"
| OP_SIZE -> "OP_SIZE"

(* Bitwise logic *)
| OP_INVERT -> "OP_INVERT"
| OP_AND -> "OP_AND"
| OP_OR -> "OP_OR"
| OP_XOR -> "OP_XOR"
| OP_EQUAL -> "OP_EQUAL"
| OP_EQUALVERIFY -> "OP_EQUALVERIFY"

(* Arithmetic*)
| OP_1ADD -> "OP_1ADD"
| OP_1SUB -> "OP_1SUB"
| OP_2MUL -> "OP_2MUL"
| OP_2DIV -> "OP_2DIV"
| OP_NEGATE -> "OP_NEGATE"
| OP_ABS -> "OP_ABS"
| OP_NOT -> "OP_NOT"
| OP_0NOTEQUAL -> "OP_0NOTEQUAL"
| OP_ADD -> "OP_ADD"
| OP_SUB -> "OP_SUB"
| OP_MUL -> "OP_MUL"
| OP_DIV -> "OP_DIV"
| OP_MOD -> "OP_MOD"
| OP_LSHIFT -> "OP_LSHIFT"
| OP_RSHIFT -> "OP_RSHIFT"
| OP_BOOLAND -> "OP_BOOLAND"
| OP_BOOLOR -> "OP_BOOLOR"
| OP_NUMEQUAL -> "OP_NUMEQUAL"
| OP_NUMEQUALVERIFY -> "OP_NUMEQUALVERIFY"
| OP_NUMNOTEQUAL -> "OP_NUMNOTEQUAL"
| OP_LESSTHAN -> "OP_LESSTHAN"
| OP_GREATERTHAN -> "OP_GREATERTHAN"
| OP_LESSTHANOREQUAL -> "OP_LESSTHANOREQUAL"
| OP_GREATERTHANOREQUAL -> "OP_GREATERTHANOREQUAL"
| OP_MIN -> "OP_MIN"
| OP_MAX -> "OP_MAX"
| OP_WITHIN -> "OP_WITHIN"

(* Crypto *)
| OP_RIPEMD160 -> "OP_RIPEMD160"
| OP_SHA1 -> "OP_SHA1"
| OP_SHA256 -> "OP_SHA256"
| OP_HASH160 -> "OP_HASH160"
| OP_HASH256 -> "OP_HASH256"
| OP_CODESEPARATOR -> "OP_CODESEPARATOR"
| OP_CHECKSIG -> "OP_CHECKSIG"
| OP_CHECKSIGVERIFY -> "OP_CHECKSIGVERIFY"
| OP_CHECKMULTISIG -> "OP_CHECKMULTISIG"
| OP_CHECKMULTISIGVERIFY -> "OP_CHECKMULTISIGVERIFY"

(* Lock time *)
| OP_CHECKLOCKTIMEVERIFY -> "OP_CHECKLOCKTIMEVERIFY"
| OP_CHECKSEQUENCEVERIFY -> "OP_CHECKSEQUENCEVERIFY"

(* Pseudo words *)
| OP_PUBKEYHASH -> "OP_PUBKEYHASH"
| OP_PUBKEY -> "OP_PUBKEY"
| OP_INVALIDOPCODE -> "OP_INVALIDOPCODE"

(* Reserved words*)
| OP_RESERVED -> "OP_RESERVED"
| OP_VER -> "OP_VER"
| OP_VERIF -> "OP_VERIF"
| OP_VERNOTIF -> "OP_VERNOTIF"
| OP_RESERVED1 -> "OP_RESERVED1"
| OP_RESERVED2 -> "OP_RESERVED2"
| OP_NOP (x) -> Printf.sprintf "OP_NOP(%x)" x
;;

let opcode_to_hex oc = 
    let rec data_to_bytearray d = match Bytes.length d with
    | 0 -> []
    | n -> (Char.code @@ Bytes.get d 0) :: data_to_bytearray (Bytes.sub d 1 (n-1))
    in
    match oc with
    (* Constants *)
    | OP_0 -> [ 0x00 ]
    | OP_FALSE -> [ 0x00 ]
    | OP_DATA (s, data) -> s :: (data_to_bytearray data)
    | OP_PUSHDATA1 (s, data) -> [ 0x4c; s ] @ (data_to_bytearray data)
    | OP_PUSHDATA2 (s1, s2, data) -> [ 0x4d; s1; s2 ] @ (data_to_bytearray data)
    | OP_PUSHDATA4 (s1, s2, s3, s4, data) -> [ 0x4e; s1; s2; s3; s4 ] @ (data_to_bytearray data)
    | OP_1NEGATE -> [ 0x4f ]
    | OP_1 -> [ 0x51 ]
    | OP_TRUE -> [ 0x51 ]
    | OP_2 -> [ 0x52 ]
    | OP_3 -> [ 0x53 ]
    | OP_4 -> [ 0x54 ]
    | OP_5 -> [ 0x55 ]
    | OP_6 -> [ 0x56 ]
    | OP_7 -> [ 0x57 ]
    | OP_8 -> [ 0x58 ]
    | OP_9 -> [ 0x59 ]
    | OP_10 -> [ 0x5a ]
    | OP_11 -> [ 0x5b ]
    | OP_12 -> [ 0x5c ]
    | OP_13 -> [ 0x5d ]
    | OP_14 -> [ 0x5e ]
    | OP_15 -> [ 0x5f ]
    | OP_16 -> [ 0x60 ]

    (* Flow *)
    | OP_IF -> [ 0x63 ]
    | OP_NOTIF -> [ 0x64 ]
    | OP_ELSE -> [ 0x67 ]
    | OP_ENDIF -> [ 0x68 ]
    | OP_VERIFY -> [ 0x69 ]
    | OP_RETURN -> [ 0x6a ]

    (* Stack *)
    | OP_TOALTSTACK -> [ 0x6b ]
    | OP_FROMALTSTACK -> [ 0x6c ]
    | OP_IFDUP -> [ 0x73 ]
    | OP_DEPTH -> [ 0x74 ]
    | OP_DROP -> [ 0x75 ]
    | OP_DUP -> [ 0x76 ]
    | OP_NIP -> [ 0x77 ]
    | OP_OVER -> [ 0x78 ]
    | OP_PICK -> [ 0x79 ]
    | OP_ROLL -> [ 0x7a ]
    | OP_ROT -> [ 0x7b ]
    | OP_SWAP -> [ 0x7c ]
    | OP_TUCK -> [ 0x7d ]
    | OP_2DROP -> [ 0x6d ]
    | OP_2DUP -> [ 0x6e ]
    | OP_3DUP -> [ 0x6f ]
    | OP_2OVER -> [ 0x70 ]
    | OP_2ROT -> [ 0x71 ]
    | OP_2SWAP -> [ 0x72 ]

    (* Splice *)
    | OP_CAT -> [ 0x7e ]
    | OP_SUBSTR -> [ 0x7f ]
    | OP_LEFT -> [ 0x80 ]
    | OP_RIGHT -> [ 0x81 ]
    | OP_SIZE -> [ 0x82 ]

    (* Bitwise logic *)
    | OP_INVERT -> [ 0x83 ]
    | OP_AND -> [ 0x84 ]
    | OP_OR -> [ 0x85 ]
    | OP_XOR -> [ 0x86 ]
    | OP_EQUAL -> [ 0x87 ]
    | OP_EQUALVERIFY -> [ 0x88 ]

    (* Arithmetic*)
    | OP_1ADD -> [ 0x8b ]
    | OP_1SUB -> [ 0x8c ]
    | OP_2MUL -> [ 0x8d ]
    | OP_2DIV -> [ 0x8e ]
    | OP_NEGATE -> [ 0x8f ]
    | OP_ABS -> [ 0x90 ]
    | OP_NOT -> [ 0x91 ]
    | OP_0NOTEQUAL -> [ 0x92 ]
    | OP_ADD -> [ 0x93 ]
    | OP_SUB -> [ 0x94 ]
    | OP_MUL -> [ 0x95 ]
    | OP_DIV -> [ 0x96 ]
    | OP_MOD -> [ 0x97 ]
    | OP_LSHIFT -> [ 0x98 ]
    | OP_RSHIFT -> [ 0x99 ]
    | OP_BOOLAND -> [ 0x9a ]
    | OP_BOOLOR -> [ 0x9b ]
    | OP_NUMEQUAL -> [ 0x9c ]
    | OP_NUMEQUALVERIFY -> [ 0x9d ]
    | OP_NUMNOTEQUAL -> [ 0x9e ]
    | OP_LESSTHAN -> [ 0x9f ]
    | OP_GREATERTHAN -> [ 0xa0 ]
    | OP_LESSTHANOREQUAL -> [ 0xa1 ]
    | OP_GREATERTHANOREQUAL -> [ 0xa2 ]
    | OP_MIN -> [ 0xa3 ]
    | OP_MAX -> [ 0xa4 ]
    | OP_WITHIN -> [ 0xa5 ]

    (* Crypto *)
    | OP_RIPEMD160 -> [ 0xa6 ]
    | OP_SHA1 -> [ 0xa7 ]
    | OP_SHA256 -> [ 0xa8 ]
    | OP_HASH160 -> [ 0xa9 ]
    | OP_HASH256 -> [ 0xaa ]
    | OP_CODESEPARATOR -> [ 0xab ]
    | OP_CHECKSIG -> [ 0xac ]
    | OP_CHECKSIGVERIFY -> [ 0xad ]
    | OP_CHECKMULTISIG -> [ 0xae ]
    | OP_CHECKMULTISIGVERIFY -> [ 0xaf ]

    (* Lock time *)
    | OP_CHECKLOCKTIMEVERIFY -> [ 0xb1 ]
    | OP_CHECKSEQUENCEVERIFY -> [ 0xb2 ]

    (* Pseudo words *)
    | OP_PUBKEYHASH -> [ 0xfd ]
    | OP_PUBKEY -> [ 0xfe ]
    | OP_INVALIDOPCODE -> [ 0xff ]

    (* Reserved words*)
    | OP_RESERVED -> [ 0x50 ]
    | OP_VER -> [ 0x62 ]
    | OP_VERIF -> [ 0x65 ]
    | OP_VERNOTIF -> [ 0x66 ]
    | OP_RESERVED1 -> [ 0x89 ]
    | OP_RESERVED2 -> [ 0x8a ]
    | OP_NOP (x) -> [ x ]
;;


let opcode_of_hex s = 
    let consume_next s =
        let c = Char.code (Bytes.get s 0) in
        let s' = Bytes.sub s 1 ((Bytes.length s) - 1) in
        (c, s')
    in
    let consume_bytes s sizea =
        let rec sizea_to_int sizea acc = match sizea with
        | [] -> acc
        | sa::sizea' -> sizea_to_int sizea' (acc * 0xFF + sa)
        in 
        let sizea = sizea_to_int sizea 0 in
        if sizea > Bytes.length s then
           (Bytes.sub s 0 (Bytes.length s), "")
        else
           (Bytes.sub s 0 sizea, Bytes.sub s sizea ((Bytes.length s) - sizea))
    in
    let c, s' = consume_next s in
    match c with 
    (* Constants *)
    | 0x00 -> (OP_0, s')
    | 0x00 -> (OP_FALSE, s')
    | x when x >= 0x01 && x <= 0x4b -> 
        let d, s'' = consume_bytes s' [x] in
        (OP_DATA (x, d), s'')
    | 0x4c -> 
        let c', s'' = consume_next s' in
        let d, s'' = consume_bytes s'' [c'] in
        (OP_PUSHDATA1 (c', d), s'')
    | 0x4d -> 
        let c', s'' = consume_next s' in
        let c'', s'' = consume_next s'' in
        let d, s'' = consume_bytes s'' [c'; c''] in
        (OP_PUSHDATA2 (c', c'', d), s'')
    | 0x4e -> 
        let c', s'' = consume_next s' in
        let c'', s'' = consume_next s'' in
        let c''', s'' = consume_next s'' in
        let c'''', s'' = consume_next s'' in
        let d, s'' = consume_bytes s'' [c'; c''; c'''; c''''] in
        (OP_PUSHDATA4 (c', c'', c''', c'''', d), s'')
    | 0x4f -> (OP_1NEGATE, s')
    | 0x51 -> (OP_1, s')
    | 0x51 -> (OP_TRUE, s')
    | 0x52 -> (OP_2, s')
    | 0x53 -> (OP_3, s')
    | 0x54 -> (OP_4, s')
    | 0x55 -> (OP_5, s')
    | 0x56 -> (OP_6, s')
    | 0x57 -> (OP_7, s')
    | 0x58 -> (OP_8, s')
    | 0x59 -> (OP_9, s')
    | 0x5a -> (OP_10, s')
    | 0x5b -> (OP_11, s')
    | 0x5c -> (OP_12, s')
    | 0x5d -> (OP_13, s')
    | 0x5e -> (OP_14, s')
    | 0x5f -> (OP_15, s')
    | 0x60 -> (OP_16, s')

    (* Flow *)
    | 0x63 -> (OP_IF, s')
    | 0x64 -> (OP_NOTIF, s')
    | 0x67 -> (OP_ELSE, s')
    | 0x68 -> (OP_ENDIF, s')
    | 0x69 -> (OP_VERIFY, s')
    | 0x6a -> (OP_RETURN, s')

    (* Stack *)
    | 0x6b -> (OP_TOALTSTACK, s')
    | 0x6c -> (OP_FROMALTSTACK, s')
    | 0x73 -> (OP_IFDUP, s')
    | 0x74 -> (OP_DEPTH, s')
    | 0x75 -> (OP_DROP, s')
    | 0x76 -> (OP_DUP, s')
    | 0x77 -> (OP_NIP, s')
    | 0x78 -> (OP_OVER, s')
    | 0x79 -> (OP_PICK, s')
    | 0x7a -> (OP_ROLL, s')
    | 0x7b -> (OP_ROT, s')
    | 0x7c -> (OP_SWAP, s')
    | 0x7d -> (OP_TUCK, s')
    | 0x6d -> (OP_2DROP, s')
    | 0x6e -> (OP_2DUP, s')
    | 0x6f -> (OP_3DUP, s')
    | 0x70 -> (OP_2OVER, s')
    | 0x71 -> (OP_2ROT, s')
    | 0x72 -> (OP_2SWAP, s')

    (* Splice *)
    | 0x7e -> (OP_CAT, s')
    | 0x7f -> (OP_SUBSTR, s')
    | 0x80 -> (OP_LEFT, s')
    | 0x81 -> (OP_RIGHT, s')
    | 0x82 -> (OP_SIZE, s')

    (* Bitwise logic *)
    | 0x83 -> (OP_INVERT, s')
    | 0x84 -> (OP_AND, s')
    | 0x85 -> (OP_OR, s')
    | 0x86 -> (OP_XOR, s')
    | 0x87 -> (OP_EQUAL, s')
    | 0x88 -> (OP_EQUALVERIFY, s')

    (* Arithmetic*)
    | 0x8b -> (OP_1ADD, s')
    | 0x8c -> (OP_1SUB, s')
    | 0x8d -> (OP_2MUL, s')
    | 0x8e -> (OP_2DIV, s')
    | 0x8f -> (OP_NEGATE, s')
    | 0x90 -> (OP_ABS, s')
    | 0x91 -> (OP_NOT, s')
    | 0x92 -> (OP_0NOTEQUAL, s')
    | 0x93 -> (OP_ADD, s')
    | 0x94 -> (OP_SUB, s')
    | 0x95 -> (OP_MUL, s')
    | 0x96 -> (OP_DIV, s')
    | 0x97 -> (OP_MOD, s')
    | 0x98 -> (OP_LSHIFT, s')
    | 0x99 -> (OP_RSHIFT, s')
    | 0x9a -> (OP_BOOLAND, s')
    | 0x9b -> (OP_BOOLOR, s')
    | 0x9c -> (OP_NUMEQUAL, s')
    | 0x9d -> (OP_NUMEQUALVERIFY, s')
    | 0x9e -> (OP_NUMNOTEQUAL, s')
    | 0x9f -> (OP_LESSTHAN, s')
    | 0xa0 -> (OP_GREATERTHAN, s')
    | 0xa1 -> (OP_LESSTHANOREQUAL, s')
    | 0xa2 -> (OP_GREATERTHANOREQUAL, s')
    | 0xa3 -> (OP_MIN, s')
    | 0xa4 -> (OP_MAX, s')
    | 0xa5 -> (OP_WITHIN, s')

    (* Crypto *)
    | 0xa6 -> (OP_RIPEMD160, s')
    | 0xa7 -> (OP_SHA1, s')
    | 0xa8 -> (OP_SHA256, s')
    | 0xa9 -> (OP_HASH160, s')
    | 0xaa -> (OP_HASH256, s')
    | 0xab -> (OP_CODESEPARATOR, s')
    | 0xac -> (OP_CHECKSIG, s')
    | 0xad -> (OP_CHECKSIGVERIFY, s')
    | 0xae -> (OP_CHECKMULTISIG, s')
    | 0xaf -> (OP_CHECKMULTISIGVERIFY, s')

    (* Lock time *)
    | 0xb1 -> (OP_CHECKLOCKTIMEVERIFY, s')
    | 0xb2 -> (OP_CHECKSEQUENCEVERIFY, s')

    (* Pseudo words *)
    | 0xfd -> (OP_PUBKEYHASH, s')
    | 0xfe -> (OP_PUBKEY, s')
    | 0xff -> (OP_INVALIDOPCODE , s')

    (* Reserved words*)
    | 0x50 -> (OP_RESERVED, s')
    | 0x62 -> (OP_VER, s')
    | 0x65 -> (OP_VERIF, s')
    | 0x66 -> (OP_VERNOTIF, s')
    | 0x89 -> (OP_RESERVED1, s')
    | 0x81 -> (OP_RESERVED2, s')


    | x when x = 0x61 || (x >= 0xb0 && x <= 0xb9) -> (OP_NOP (x), s')
    | x -> (OP_NOP (x), s')

;;

(* 
    https://github.com/bitcoin/bitcoin/blob/master/src/script/interpreter.cpp 
    https://en.bitcoin.it/wiki/Script
*)
let rec _eval st altst scr = 
    let rec to_endif_or_else st altst scr = match scr with
    | [] -> false
    | op::scr' -> match op with
        | OP_ENDIF -> _eval st altst scr'
        | OP_ELSE -> _eval st altst scr'
        | _ -> to_endif_or_else st altst scr'
    in
    let rec to_endif st altst scr = match scr with
    | [] -> false
    | op::scr' -> match op with
        | OP_ENDIF -> _eval st altst scr'
        | _ -> to_endif st altst scr'
    in
    match scr with
    | [] -> (SStack.popi st) <> 0
    | op::scr' -> match op with
        (* Constants *)
        | OP_0
        | OP_FALSE -> SStack.push 0 st; _eval st altst scr'
        | OP_DATA (s, data) -> SStack.push data st; _eval st altst scr'
        | OP_PUSHDATA1 (s, data) -> SStack.push data st; _eval st altst scr'
        | OP_PUSHDATA2 (a, b, data) -> SStack.push data st; _eval st altst scr'
        | OP_PUSHDATA4 (a, b, c, d, data) -> SStack.push data st; _eval st altst scr'
        | OP_1NEGATE -> SStack.pushi (-1) st; _eval st altst scr'
        | OP_1
        | OP_TRUE -> SStack.pushi 1 st; _eval st altst scr'
        | OP_2 -> SStack.pushi 2 st; _eval st altst scr'
        | OP_3 -> SStack.pushi 3 st; _eval st altst scr'
        | OP_4 -> SStack.pushi 4 st; _eval st altst scr'
        | OP_5 -> SStack.pushi 5 st; _eval st altst scr'
        | OP_6 -> SStack.pushi 6 st; _eval st altst scr'
        | OP_7 -> SStack.pushi 7 st; _eval st altst scr'
        | OP_8 -> SStack.pushi 8 st; _eval st altst scr'
        | OP_9 -> SStack.pushi 9 st; _eval st altst scr'
        | OP_10 -> SStack.pushi 10 st; _eval st altst scr'
        | OP_11 -> SStack.pushi 11 st; _eval st altst scr'
        | OP_12 -> SStack.pushi 12 st; _eval st altst scr'
        | OP_13 -> SStack.pushi 13 st; _eval st altst scr'
        | OP_14 -> SStack.pushi 14 st; _eval st altst scr'
        | OP_15 -> SStack.pushi 15 st; _eval st altst scr'
        | OP_16 -> SStack.pushi 16 st; _eval st altst scr'

        (* Flow *)
        | OP_IF -> if SStack.popi st <> 0 then _eval st altst scr' else to_endif_or_else st altst scr'
        | OP_NOTIF -> if SStack.popi st = 0 then _eval st altst scr' else to_endif_or_else st altst scr'
        | OP_ELSE -> to_endif st altst scr'
        | OP_ENDIF -> _eval st altst scr'
        | OP_VERIFY -> if SStack.popi st <> 0 then true else false
        | OP_RETURN -> false

        (* Stack *)
        | OP_TOALTSTACK -> 
            let ap = SStack.pop st in SStack.push ap altst; 
            _eval st altst scr'
        | OP_FROMALTSTACK -> 
            let ap = SStack.pop altst in SStack.push ap st; 
            _eval st altst scr'
        | OP_IFDUP -> true
        | OP_DEPTH -> true
        | OP_DROP -> 
            SStack.pop st |> ignore;
            _eval st altst scr'
        | OP_DUP -> 
            let ap = SStack.top st in SStack.push ap st; 
            _eval st altst scr'
        | OP_NIP -> true
        | OP_OVER -> true
        | OP_PICK -> true
        | OP_ROLL -> true
        | OP_ROT -> true
        | OP_SWAP -> true
        | OP_TUCK -> true
        | OP_2DROP -> true
        | OP_2DUP -> true
        | OP_3DUP -> true
        | OP_2OVER -> true
        | OP_2ROT -> true
        | OP_2SWAP -> true

        (* Splice *)
        | OP_SIZE -> true

        (* Bitwise logic *)
        | OP_EQUAL -> 
            let a = SStack.top st in 
            let b = SStack.top st in 
            (if a = b then SStack.push 1 st else SStack.push 0 st);
            _eval st altst scr' 
        | OP_EQUALVERIFY -> 
            let a = SStack.top st in 
            let b = SStack.top st in 
            if a = b then true else false

        (* Arithmetic*)
        | OP_1ADD -> 
            let ap = SStack.topi st in SStack.pushi (ap + 1) st; 
            _eval st altst scr' 
        | OP_1SUB -> 
            let ap = SStack.topi st in SStack.pushi (ap - 1) st; 
            _eval st altst scr' 
        | OP_NEGATE -> 
            let ap = SStack.topi st in SStack.pushi (-ap) st; 
            _eval st altst scr'        
        | OP_ABS -> 
            let ap = SStack.topi st in SStack.pushi (abs ap) st; 
            _eval st altst scr' 
        | OP_NOT -> 
            let ap = SStack.topi st in 
            if ap = 0 then SStack.pushi 1 st else SStack.pushi 0 st;
            _eval st altst scr' 
        | OP_0NOTEQUAL -> 
            let ap = SStack.topi st in 
            if ap = 0 then SStack.pushi 0 st else SStack.pushi 1 st;
            _eval st altst scr' 
        | OP_ADD -> 
            let a = SStack.topi st in 
            let b = SStack.topi st in 
            SStack.pushi (a + b) st; 
            _eval st altst scr'      
        | OP_SUB -> 
            let a = SStack.topi st in 
            let b = SStack.topi st in 
            SStack.pushi (a - b) st; 
            _eval st altst scr'   
        | OP_BOOLAND -> 
            let a = SStack.topi st in 
            let b = SStack.topi st in 
            (if a <> 0 && b <> 0 then SStack.pushi 1 st else SStack.pushi 0 st);
            _eval st altst scr'  
        | OP_BOOLOR -> 
            let a = SStack.topi st in 
            let b = SStack.topi st in 
            (if a <> 0 || b <> 0 then SStack.pushi 1 st else SStack.pushi 0 st);
            _eval st altst scr'  
        | OP_NUMEQUAL -> 
            let a = SStack.topi st in 
            let b = SStack.topi st in 
            (if a = b then SStack.pushi 1 st else SStack.pushi 0 st);
            _eval st altst scr'  
        | OP_NUMEQUALVERIFY -> 
            let a = SStack.topi st in 
            let b = SStack.topi st in 
            (if a = b then true else false);
            _eval st altst scr' 
        | OP_NUMNOTEQUAL -> 
            let a = SStack.topi st in 
            let b = SStack.topi st in 
            (if a <> b then SStack.pushi 1 st else SStack.pushi 0 st);
            _eval st altst scr'  
        | OP_LESSTHAN -> 
            let a = SStack.topi st in 
            let b = SStack.topi st in 
            (if a < b then SStack.pushi 1 st else SStack.pushi 0 st);
            _eval st altst scr'  
        | OP_GREATERTHAN -> 
            let a = SStack.topi st in 
            let b = SStack.topi st in 
            (if a > b then SStack.pushi 1 st else SStack.pushi 0 st);
            _eval st altst scr'  
        | OP_LESSTHANOREQUAL -> 
            let a = SStack.topi st in 
            let b = SStack.topi st in 
            (if a <= b then SStack.pushi 1 st else SStack.pushi 0 st);
            _eval st altst scr'  
        | OP_GREATERTHANOREQUAL -> 
            let a = SStack.topi st in 
            let b = SStack.topi st in 
            (if a >= b then SStack.pushi 1 st else SStack.pushi 0 st);
            _eval st altst scr'  
        | OP_MIN -> 
            let a = SStack.topi st in 
            let b = SStack.topi st in 
            (if a > b then SStack.pushi b st else SStack.pushi a st);
            _eval st altst scr' 
        | OP_MAX -> 
            let a = SStack.topi st in 
            let b = SStack.topi st in 
            (if a > b then SStack.pushi a st else SStack.pushi b st);
            _eval st altst scr' 
        | OP_WITHIN -> 
            let x = SStack.topi st in 
            let min = SStack.topi st in 
            let max = SStack.topi st in 
            (if x >= min && x < max then SStack.pushi 1 st else SStack.pushi 0 st);
            _eval st altst scr' 

        (* Crypto *)
        | OP_RIPEMD160 -> true
        | OP_SHA1 -> true
        | OP_SHA256 -> true
        | OP_HASH160 -> true
        | OP_HASH256 -> true
        | OP_CODESEPARATOR -> true
        | OP_CHECKSIG -> true
        | OP_CHECKSIGVERIFY -> true
        | OP_CHECKMULTISIG -> true
        | OP_CHECKMULTISIGVERIFY -> true

        (* Lock time *)
        | OP_CHECKLOCKTIMEVERIFY -> true
        | OP_CHECKSEQUENCEVERIFY -> true

        (* Pseudo words *)
        | OP_PUBKEYHASH -> true
        | OP_PUBKEY -> true
        | OP_INVALIDOPCODE -> true

        (* Reserved words*)
        | OP_RESERVED -> true
        | OP_VER -> true
        | OP_VERIF -> true
        | OP_VERNOTIF -> true
        | OP_RESERVED1 -> true
        | OP_RESERVED2 -> true

        | OP_NOP (x) -> _eval st altst scr'
        | _ -> false
;;


let to_string scr =
    let rec tos' ops = match ops with
    | [] -> ""
    | o::ops' -> (opcode_to_string o) ^ " " ^ (tos' ops')
    in tos' (fst scr)
;;

let eval scr = 
    let st = SStack.create () in
    let altst = SStack.create () in
    try _eval st altst (fst scr) with _ -> false
;;

let join s1 s2 = ((fst s1) @ (fst s2), (snd s1) + (snd s2));;

let length scr = snd scr;;

let serialize scr = 
    let rec serialize' scr = match scr with
    | [] -> ""
    | op::scr' ->
        let rec hlist_to_bytes il = match il with
        | [] -> ""
        | i::il' -> (Bytes.make 1 (Char.chr i)) ^ (hlist_to_bytes il')
        in
        let r = hlist_to_bytes @@ opcode_to_hex op in
        r ^ (serialize' scr')
    in 
    let s = serialize' (fst scr) in
    match (snd scr, Bytes.length s) with
    | (n, n') when n = n' -> s 
    | (n, n') -> failwith "Wrong serialize size"
;;

let parse s = 
    (*Printf.printf "%s\n%!" @@ Hash.print_bin s;*)
    let rec parse' s = match Bytes.length s with
    | 0 -> []
    | n -> 
        let op, s' = opcode_of_hex s in 
        (*(Printf.printf "%s\n%!" @@ opcode_to_string op);*)
        op :: (parse' s')
    in (parse' s, String.length s)
;;