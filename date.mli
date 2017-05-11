module type DATE =
  sig
    val new_day : int
    val new_month : int
    val new_year : int

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
module Date : DATE
