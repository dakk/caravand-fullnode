open Network;;
open Log;;
open Params;;

let main () =
	Log.info "letchain" "Starting 0.1";
	let cn = Params.BTC in
	Log.info "letchain" "Selected network: %s" (name_of_network cn);
	let p = params_of_network cn in
	let n = Network.init p in ()
;;

main ();;
