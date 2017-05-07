open Date
open Hour
open Cities
open Train

(*module MyTgvTrain = MyTrain (TgvTrain);;
module MyEuroTrain = MyTrain (EuroTrain);;*)

let train_list = []

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
      | head::tail when acc = 0 -> if ((String.equal head "create" = true || String.equal head "list" || String.equal head "delete") && aux (acc + 1) tail = true) then true else false
      | head::tail when acc = 1 -> if ((String.equal head "TGV" = true || String.equal head "Thalys" || String.equal head "Eurostar") && aux (acc + 1) tail = true) then true else false
      | head::tail when acc = 2 -> if (Date.date_is_valid head = true && aux (acc + 1) tail = true) then true else false
      | head::tail when acc = 3 -> if (Hour.hour_is_valid head = true && aux (acc + 1) tail = true) then true else false
      | head::tail when acc = 4 -> if Cities.cities_are_valid head = true then true else false
      | _ -> false
in aux 0 list;;

let _ =
while true do
  let str = read_line () in
    let empty = []
      in
    let res = create_newlist empty (Str.split (Str.regexp "[ \t]") str) in
    if check_argument res = true
      then Train.print_all_train  (append_item train_list (Train.create_train (List.nth res 1)))(* ; Train.print_all_train train_list*)
    else
      print_endline ""
done;
;;

(*print_endline (Hour.calculate_hour (Str.split (Str.regexp ":") "07:55") 100)*)

(*
v = d/t
t = d/v
d = v * t
*)
