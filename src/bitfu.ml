open Dns;;
open Log;;

let main () =
	Log.set_log_level Log.DEBUG;
	Log.set_output stdout;
	Log.color_on ();
	Log.info "%s" "BitFU 0.1";

	Dns.query "seed.bitcoin.sipa.be";
;;

main ();;
