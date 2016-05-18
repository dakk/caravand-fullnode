open Network;;
open Log;;

let main () =
	Log.info "~" "BitFU 0.1";
	let n = Network.init () in ()
;;

main ();;
