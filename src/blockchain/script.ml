type t = string;;

type opcode = 
(* Constants *)
| OP_0
| OP_FALSE
| OP_DATA of int
| OP_PUSHDATA1 of int
| OP_PUSHDATA2 of int * int
| OP_PUSHDATA4 of int * int * int * int
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
| OP_NOP
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
| OP_3DROP
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
| OP_NOP1
| OP_NOP4
| OP_NOP5
| OP_NOP6
| OP_NOP7
| OP_NOP8
| OP_NOP9
| OP_NOP10
;;


let opcode_to_hex oc = match oc with
(* Constants *)
| OP_0 -> [ 0x00 ]
| OP_FALSE -> [ 0x00 ]
| OP_DATA (d) -> [ d ]
| OP_PUSHDATA1 (d) -> [ 0x4c; d ]
| OP_PUSHDATA2 (a, b) -> [ 0x4d; a; b ]
| OP_PUSHDATA4 (a, b, c, d) -> [ 0x4e; a; b; c; d ]
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
| OP_NOP -> [ 0x61 ]
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
| OP_CHECKSIGVERIFY -> [ 0xd ]
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
| OP_NOP1 -> [ 0xb0 ]
| OP_NOP4 -> [ 0xb3 ]
| OP_NOP5 -> [ 0xb4 ]
| OP_NOP6 -> [ 0xb5 ]
| OP_NOP7 -> [ 0xb6 ]
| OP_NOP8 -> [ 0xb7 ]
| OP_NOP9 -> [ 0xb8 ]
| OP_NOP10 -> [ 0xb9 ]
;;


let opcode_of_hex s = 
    let consume_next s =
        let c = Char.code (Bytes.get s 0) in
        let s' = Bytes.sub s 1 ((Bytes.length s) - 1) in
        (c, s')
    in
    let c, s' = consume_next s in
    match c with 
    (* Constants *)
    | 0x00 -> (OP_0, s')
    | 0x00 -> (OP_FALSE, s')
    | x when x >= 0x01 && x <= 0x4b -> (OP_DATA (x), s')
    | 0x4c -> 
        let c', s'' = consume_next s' in
        (OP_PUSHDATA1 (c'), s'')
    | 0x4d -> 
        let c', s'' = consume_next s' in
        let c'', s''' = consume_next s'' in
        (OP_PUSHDATA2 (c', c''), s''')
    | 0x4e -> 
        let c', s'' = consume_next s' in
        let c'', s'' = consume_next s'' in
        let c''', s'' = consume_next s'' in
        let c'''', s'' = consume_next s'' in
        (OP_PUSHDATA4 (c', c'', c''', c'''), s'')
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
    
    | 0x61 -> (OP_NOP, s')

    (* Reserved words*)
    | 0x50 -> (OP_RESERVED, s')
    | 0x62 -> (OP_VER, s')
    | 0x65 -> (OP_VERIF, s')
    | 0x66 -> (OP_VERNOTIF, s')
    | 0x89 -> (OP_RESERVED1, s')
    | 0x81 -> (OP_RESERVED2, s')
    | 0xb0 -> (OP_NOP1, s')
    | 0xb3 -> (OP_NOP4, s')
    | 0xb4 -> (OP_NOP5, s')
    | 0xb5 -> (OP_NOP6, s')
    | 0xb6 -> (OP_NOP7, s')
    | 0xb7 -> (OP_NOP8, s')
    | 0xb8 -> (OP_NOP9, s')
    | 0xb9 -> (OP_NOP10, s')

    | _ -> (OP_INVALIDOPCODE, s')
;;

let eval s = true;;

let length s = String.length s;;

let serialize s = s;;

let parse s = Some (s);;