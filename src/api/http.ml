open Core.Std;;
open Async.Std;;
open Cohttp_async;;

(* We can implement this with a simple Tcp server with yojson parsing? *)

let handler ~body:_ _sock req =
	let uri = Cohttp.Request.uri req in
	match Uri.path uri with
	| "/test" ->
		Uri.get_query_param uri "hello"
		|> Option.map ~f:(fun v -> "hello: " ^ v)
		|> Option.value ~default:"No param hello supplied"
		|> Server.respond_with_string
	| _ ->
		Server.respond_with_string ~code:`Not_found "Route not found"
;;

let start_server port () =
	Cohttp_async.Server.create ~on_handler_error:`Raise (Tcp.on_port port) handler 
	>>= fun _ -> Deferred.never ()
;;

let loop bc =
	let start' () =
	Command.async
		~summary:""
		Command.Spec.(empty +>
		flag "-p" (optional_with_default 8085 int)
			~doc:"Port to listen for api requests"
		) start_server
		|> Command.run
	in ()
;;