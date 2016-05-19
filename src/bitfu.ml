open Network;;
open Log;;
open Params;;

let main () =
	Log.info "Bitfu" "Starting 0.1";
	let cn = Params.BTC in
	Log.info "Bitfu" "Selected network: %s" (name_of_network cn);
	let p = params_of_network cn in
	let n = Network.init p in ()
;;

main ();;
