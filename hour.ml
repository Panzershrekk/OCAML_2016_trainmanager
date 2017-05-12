module type HOUR =
  sig
    val string_to_int_cmp : string -> int -> int
    val verify_hour : string list -> bool
    val hour_is_valid : string -> bool
    val add_minute : int -> string
    val add_hour : int -> string
    val round_value : float -> float
    val calculate_hour : string list -> int -> string
    val calculate_hour_with_distance : string -> int -> string -> string
  end

module Hour : HOUR =
  struct

  let string_to_int_cmp str default =
    try
      (int_of_string str)
    with
      | Failure _ -> default;;

  let verify_hour hour_list =
    if (string_to_int_cmp (List.hd hour_list) ~-1) >= 0 && (string_to_int_cmp (List.hd hour_list) ~-1) <= 23 &&
      (string_to_int_cmp (List.nth hour_list 1) ~-1) >= 0 && (string_to_int_cmp (List.nth hour_list 1) ~-1) <= 59
    then true else false;;

  let hour_is_valid hour = verify_hour (Str.split (Str.regexp ":") hour);;

  let add_minute minute = if minute < 10 then "0" ^ string_of_int minute else string_of_int minute;;

  let add_hour hour = if hour mod 24 < 10  then "0" ^ string_of_int (hour mod 24) else string_of_int (hour mod 24)

  let round_value f = floor (f +. 0.5)

  let calculate_hour hour add = add_hour ((string_to_int_cmp (List.hd hour) ~-1) + ((add + (string_to_int_cmp (List.nth hour 1) ~-1)) / 60)) ^ ":" ^
                                add_minute (((string_to_int_cmp (List.nth hour 1) ~-1) + add) mod 60);;

  let calculate_hour_with_distance hour distance train =
      if train = "TGV"  then calculate_hour (Str.split (Str.regexp ":") hour)
        (int_of_float (round_value ((float_of_int distance) /. 230.0 *. 60.0))) else
      if train = "Thalys"  then calculate_hour (Str.split (Str.regexp ":") hour)
        (int_of_float (round_value ((float_of_int distance) /. 210.0 *. 60.0))) else
      if train = "Eurostar"  then calculate_hour (Str.split (Str.regexp ":") hour)
        (int_of_float (round_value ((float_of_int distance) /. 160.0 *. 60.0)))
      else hour
end;;
