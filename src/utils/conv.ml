let byten_to_string b = match b with
| b' when b < 1024 -> Printf.sprintf "%dB" b'
| b' when b < 1024 * 1024 -> Printf.sprintf "%dKB" (b' / 1024)
| b' -> Printf.sprintf "%dMB" (b' / 1024 / 1024)
;;
