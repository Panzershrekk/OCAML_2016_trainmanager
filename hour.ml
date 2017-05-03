module type HOUR =
  sig
    val string_to_int_cmp : string -> int -> int
    val verify_hour : string list -> bool
    val hour_is_valid : string -> bool
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

end;;
