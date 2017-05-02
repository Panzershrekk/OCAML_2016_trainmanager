module type DATE =
  sig

    type day = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday

    type month = January | February | March | April | May | June | July | August | September | October | November | December;;

    type date = day * int * month * int

    val next_day : day -> day
    val next_month : month -> month
    val is_leap_year : int -> bool
    val nb_days : int -> int -> int
    val string_to_int_cmp : string -> int -> int
    val verify_date : string list -> bool
    val date_is_valid : string -> bool
  end

module Date : DATE =
  struct

  type day = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday

  type month = January | February | March | April | May | June | July | August | September | October | November | December;;

  type date = day * int * month * int

  let next_day = function
    | Monday -> Tuesday
    | Tuesday -> Wednesday
    | Wednesday -> Thursday
    | Thursday -> Friday
    | Friday -> Saturday
    | Saturday -> Sunday
    | Sunday -> Monday;;

  let next_month = function
    | January -> February
    | February -> March
    | March -> April
    | April -> May
    | May -> June
    | June -> July
    | July -> August
    | August -> September
    | September -> October
    | October -> November
    | November -> December
    | December -> January;;

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
    if (string_to_int_cmp (List.hd info_date) ~-1) >= 1 && (string_to_int_cmp (List.hd info_date) ~-1) <= nb_days (string_to_int_cmp (List.nth info_date 2) ~-1) (string_to_int_cmp (List.nth info_date 1) ~-1) &&
      (string_to_int_cmp (List.nth info_date 1) ~-1) >= 1 && (string_to_int_cmp (List.nth info_date 1) ~-1) <= 12 &&
      (string_to_int_cmp (List.nth info_date 2) ~-1) >= 0 && (string_to_int_cmp (List.nth info_date 2) ~-1) <= 9999
    then true else false;;

  let date_is_valid date = verify_date (Str.split (Str.regexp "-") date)
end;;
