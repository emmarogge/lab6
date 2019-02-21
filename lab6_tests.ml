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

let test_neighbors () = 
	let my_address = {mailbox=1201; street=MassAve; zipcode="02138"} in
	let my_house = House my_address in
	let neighbor_house = House my_address in 
	unit_test((neighbors my_house neighbor_house) = true) "neighbors true";

	let far_away_address = {mailbox=1201; street=MainStreet; zipcode="02138"} in
	let my_house = House my_address in
	let not_neighbor_house = House far_away_address in 
	unit_test((neighbors my_house not_neighbor_house) = false) "neighbors false street_mismatch";

	let diff_zip_address = {mailbox=1201; street=MassAve; zipcode="45140"} in
	let my_house = House my_address in
	let not_neighbor_house = Apartment (7, diff_zip_address) in 
	unit_test((neighbors my_house not_neighbor_house) = false) "neighbors false zip_mismatch";;

let  test_close_to_seven () =
	let close_address = {mailbox = 8; street = MassAve ; zipcode = "02138"} in
	let close_res = House close_address in
	let far_address  = {mailbox = 3; street = MassAve; zipcode = "02138"} in
	let far_res = House far_address in
	unit_test ((close_to_seven far_res close_res) = close_res) "close_to_seven";;

let test_choose_residence () =
	let my_address = {mailbox=9; street=MassAve; zipcode="02138"} in
	let my_house = House my_address in
	let neighbor_house = House my_address in 

	let far_away_address = {mailbox=12; street=MainStreet; zipcode="02138"} in
	let not_neighbor_house = Apartment (1, far_away_address) in 

	let diff_zip_address = {mailbox=6; street=MassAve; zipcode="45140"} in
	let closest_house = House diff_zip_address in 
	let my_res_lst = [my_house;neighbor_house;not_neighbor_house;closest_house] in
	unit_test((choose_residence my_res_lst) = Some closest_house) "choose_residence";;

let test_all () = 
	test_valid_zip ();
	test_valid_residence ();
	test_neighbors ();
	test_close_to_seven ();
	test_choose_residence ();;

let _ = test_all ();;