open Big_int;;

let bitcoin_alphabet = "123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz";;
let big_base = Big_int.big_int_of_int 58;;

let bytes_to_bigint data =
    let rec btb data acc= 
        let c' = Bytes.get data 0 |> Char.code in 
        match Bytes.length data with
        | 1 -> add_int_big_int c' acc
        | n -> 
            let rest = Bytes.sub data 1 (Bytes.length data - 1) in
            btb rest @@ add_int_big_int c' (mult_int_big_int 0xFF acc)
    in btb data Big_int.zero_big_int
;;

let encode_big_int big =
  let rec encode ls = fun x -> 
    if (Big_int.eq_big_int x Big_int.zero_big_int) 
    then (match ls with 
      | [] -> "1"
      | _ -> (String.concat "" ls))
    else 
      let (q,index) = Big_int.quomod_big_int x big_base in
      (encode ((String.make 1 bitcoin_alphabet.[(Big_int.int_of_big_int index)]) :: ls) q)
  in encode [] big
;;

let encode_check data = 
    let bi = bytes_to_bigint data in
    Printf.printf "%s\n%!" (string_of_big_int bi);
    encode_big_int bi
;;