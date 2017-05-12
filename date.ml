module type DATE =
  sig

    val calculate_day : int -> int -> int -> int -> string
    val calculate_month : int -> int -> int -> int -> string
    val calculate_year : int -> int -> int -> int -> string

    val is_leap_year : int -> bool
    val nb_days : int -> int -> int
    val string_to_int_cmp : string -> int -> int
    val verify_date : string list -> bool
    val date_is_valid : string -> bool
    val calculate_date_with_int : int -> int -> int -> int -> string
    val calculate_date : string list -> int -> string
  end

module Date : DATE =
  struct

  let is_leap_year date =
    if date mod 4 = 0 && date mod 100 <> 0 || date mod 400 = 0 then true else false;;

  let nb_days date = function
    | 1 -> 31
    | 2 -> if is_leap_year date = true then 29 else 28
    | 3 -> 31
    | 4 -> 30
    | 5 -> 31
    | 6 -> 30
    | 7 -> 31
    | 8 -> 31
    | 9 -> 30
    | 10 -> 31
    | 11 -> 30
    | 12 -> 31
    | _ -> 0;;

  let string_to_int_cmp str default =
    try
      (int_of_string str)
    with
      | Failure _ -> default

  let verify_date info_date =
    if (string_to_int_cmp (List.hd info_date) ~-1) >= 1 &&
      (string_to_int_cmp (List.hd info_date) ~-1) <= nb_days (string_to_int_cmp (List.nth info_date 2) ~-1) (string_to_int_cmp (List.nth info_date 1) ~-1)
      && (string_to_int_cmp (List.nth info_date 1) ~-1) >= 1 && (string_to_int_cmp (List.nth info_date 1) ~-1) <= 12
      && (string_to_int_cmp (List.nth info_date 2) ~-1) >= 1900 && (string_to_int_cmp (List.nth info_date 2) ~-1) <= 9999
      && String.length (List.nth info_date 0) = 2 && String.length (List.nth info_date 1) = 2
    then true else false;;

  let date_is_valid date = verify_date (Str.split (Str.regexp "-") date);;

  let calculate_day day month year add = if (day + add) > (nb_days year month) then "01"
    else (if day + add < 10 then "0" ^ (string_of_int (day + add)) else string_of_int (day + add))

  let calculate_month day month year add = if ((day + add) > (nb_days year month) && (month + 1) > 12 )
    then "01" else (if month < 10 then "0" ^ (string_of_int (month)) else string_of_int (month))

  let calculate_year day month year add = if month = 12 && (calculate_month day month year add) = "01"
                                          then string_of_int (year + 1) else string_of_int year


  let calculate_date_with_int day month year add  = (calculate_day day month year add) ^ "-" ^
                                                    (calculate_month day month year add) ^ "-" ^
                                                    (calculate_year day month year add)

  let calculate_date date add = calculate_date_with_int
                                (string_to_int_cmp (List.nth date 0) ~-1)
                                (string_to_int_cmp (List.nth date 1) ~-1)
                                (string_to_int_cmp (List.nth date 2) ~-1)
                                add;;
end;;
