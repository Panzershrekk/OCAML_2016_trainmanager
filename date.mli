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
module Date : DATE
