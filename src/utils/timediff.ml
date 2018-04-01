type t = {
	years 	: int;
	months	: int;
	days  	: int;
	hours 	: int;
	minutes	: int;
};;

let diff a b = 
	let minutes = int_of_float ((a -. b) /. 60.) in
	let years = (minutes / 60 / 24 / 31 / 12) in
	let months = (minutes / 60 / 24 / 31) mod 12 in
	let days = (minutes / 60 / 24) mod 31 in
	let hours = (minutes / 60) mod 24 in
	let minutes = (minutes) mod 60 in
	{ years= years; months= months; days= days; hours= hours; minutes= minutes }
;;


let diffstring ?munit:(munit="full") a b =
	let df = diff a b in
	match munit with
	| "days" -> Printf.sprintf "%d days" (df.years * 365 + df.months * 31 + df.days)
	| "weeks" -> Printf.sprintf "%d weeks" ((df.years * 365 + df.months * 31 + df.days) / 7)
	| "full"
	| _ -> Printf.sprintf "%d y, %d m, %d d, %d h, %d m" df.years df.months df.days df.hours df.minutes
;;
				