open Date
open Hour
open Cities
open Train
open Trip

module MyTrip = MakeTrip (Train);;

let trip_list = []
let empty = []

let rec remove_trip l n = match l with
    | [] -> []
    | head::tail -> if n = (Train.get_type (MyTrip.get_train head) ^ Train.get_id (MyTrip.get_train head)) then tail else head::remove_trip tail (n);;

let rec print_my_list list_str = match list_str with
  | [] -> ()
  | head::tail -> print_endline head ; print_my_list tail;;

let append_item lst a = lst @ [a]

let rec create_newlist new_list = function
  | [] -> List.rev new_list
  | head::tail when head = "" -> create_newlist new_list tail
  | head::tail -> let tmp = head :: new_list in create_newlist tmp tail;;

let check_argument list =
    let rec aux acc = function
      | head::tail when List.length list >= 6 -> false
      | head::tail when acc = 0 && List.length list = 1 -> if (String.equal head "list" = true) || (String.equal head "quit" = true) then true else false
      | head::tail when acc = 0 && List.length list = 2 -> if String.equal head "delete" = true then true else false
      | head::tail when acc = 0 -> if (String.equal head "create" = true && aux (acc + 1) tail = true) then true else false
      | head::tail when acc = 1 -> if ((String.equal head "TGV" = true || String.equal head "Thalys" || String.equal head "Eurostar") && aux (acc + 1) tail = true) then true else false
      | head::tail when acc = 2 -> if (Date.date_is_valid head = true && aux (acc + 1) tail = true) then true else false
      | head::tail when acc = 3 -> if (Hour.hour_is_valid head = true && aux (acc + 1) tail = true) then true else false
      | head::tail when acc = 4 && List.nth list 1 = "TGV" && List.length ((Str.split (Str.regexp ",") head)) > 1 -> if Cities.cities_are_valid head = true then true else false
      | head::tail when acc = 4 && List.nth list 1 = "Thalys" && List.length ((Str.split (Str.regexp ",") head)) > 1 -> if Cities.cities_are_valid_for_thalys head = true then true else false
      | head::tail when acc = 4 && List.nth list 1 = "Eurostar" && List.length ((Str.split (Str.regexp ",") head)) > 1 -> if Cities.cities_are_valid_for_Eurostar head = true then true else false
      | _ -> false
in aux 0 list;;

let check_command str train_list = match (List.nth str 0) with
  | "create" -> append_item train_list (MyTrip.create_whole (Train.create_train (List.nth str 1)) (MyTrip.create_trip (List.nth str 2) (List.nth str 3) (List.nth str 1) (Str.split (Str.regexp ",") (List.nth str 4))))
  | "list" -> MyTrip.print_whole_list train_list ; train_list
  | "delete" -> remove_trip train_list (List.nth str 1)
  | "quit" -> exit 0
  | _ -> train_list

let rec train_manager train_list =
let str = read_line ()
in
  let res = create_newlist empty (Str.bounded_split (Str.regexp "[ \t]+") str 5)
in
  if check_argument res = true
    then
      train_manager (check_command res train_list)
    else
      train_manager train_list
;;

let _  = Random.self_init() ;
	train_manager [];;
