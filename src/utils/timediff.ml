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
