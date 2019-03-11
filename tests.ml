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

let test_record_residence () =
  let my_address = {mailbox=9; street=MassAve; zipcode="02138"} in
  let my_res = House my_address in
  let my_name = "Emma Rogge" in
  unit_test ((record_residence my_res my_name) = {residence=my_res; name=my_name}) "record_residence";;

let test_named_neighbors () =
  let my_address = {mailbox=1; street=MassAve; zipcode="02138"} in
  let my_house = House my_address in
  let neighbor_house = House my_address in

  let far_away_address = {mailbox=12; street=MainStreet; zipcode="02138"} in
  let not_neighbor_house = Apartment (1, far_away_address) in

  let neighbor_address = {mailbox=1; street=MassAve; zipcode="02138"} in
  let closest_house = House neighbor_address in

  let town_records = [] in
  let a = (record_residence my_house "Emma") in
  let b = (record_residence neighbor_house "Neighbor #1") in
  let c = (record_residence not_neighbor_house "Not a Neighbor") in
  let d = (record_residence closest_house "Neighbor #2") in
  let town_records = [a;b;c;d] in
  unit_test
    ((named_neighbors "Emma" "Neighbor #1" town_records) = true)
    "named_neighbors true";
  unit_test
    ((named_neighbors "Emma" "Neighbor #2" town_records) = true)
    "named_neighbors true";
  unit_test
    ((named_neighbors "Not a Neighbor" "Neighbor #1" town_records) = false)
    "named_neighbors false";;

(*Part II Unit Tests*)
let test_leaf_count () =
  let leaf = Leaf in
  let t = Node (1, leaf, leaf) in
  unit_test ((leaf_count t) = 2) "leaf_count small";
  unit_test (leaf_count (Node (3, t, t)) = 4) "leaf_count bigger";;

let test_find () =
  let leaf = Leaf in
  let t = Node (1, leaf, leaf) in
  let t = Node (3, t, t) in
  let t = Node (5, t, leaf) in
  unit_test (not (find 7 t)) "find false";
  unit_test (find 3 t) "find true";;

let test_min_value () =
  let leaf = Leaf in
  let t = Node (1, leaf, leaf) in
  let t' = Node (~- 1, leaf, leaf) in
  unit_test ((min_value t) = Some 1) "min_value leaf";
  let t = Node (3, t, t') in
  let t = Node (5, t, leaf) in
  unit_test ((min_value t) = Some ~-1) "min_value tree";;

let test_map_tree () =
  let leaf = Leaf in
  let t = Node (1, leaf, leaf) in
  let t' = Node (~- 1, leaf, leaf) in
  unit_test ((map_tree (fun x -> ( * ) x 10) t)
             = Node (10, leaf, leaf)) "map_tree int_product";
  let t = Node ("emma", leaf, leaf) in
  let t = Node ("edgar", t, leaf) in
  unit_test (((map_tree) (fun s -> "hello, " ^ s) t)
             = Node ("hello, edgar", Node ("hello, emma", leaf, leaf), leaf))
    "map_tree concat_string";;

let test_all () =
  print_endline "=======Part I=======";
  test_valid_zip ();
  test_valid_residence ();
  test_neighbors ();
  test_close_to_seven ();
  test_choose_residence ();
  test_record_residence ();
  test_named_neighbors ();
  print_endline "";
  print_endline "=======Part II=======";
  test_leaf_count ();
  test_find();
  test_min_value();
  test_map_tree();
  print_endline "";;

let _ = test_all ();;