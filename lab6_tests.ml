open Lab6;;
open CS51;;

(*Part I Unit Tests*)
let test_valid_zip () =
	unit_test(valid_zip("12345") = true) "valid_zip true";
	unit_test(valid_zip("~-123") = false) "valid_zip false";;

let test_valid_residence () =
	let my_address = {mailbox=1201; street=MassAve; zipcode="02138"} in
	let my_house = House my_address in
	unit_test((valid_residence my_house) = true) "valid_residence  true";

	let bad_address = {mailbox= ~-12; street=MassAve; zipcode="02138"} in
	let bad_house = Apartment (7, bad_address) in
	unit_test((valid_residence bad_house) = false) "valid_residence false";;

let test_all () = 
	test_valid_zip ();
	test_valid_residence ();;

let _ = test_all ();;