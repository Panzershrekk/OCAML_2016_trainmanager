module type DATE =
  sig
    val is_leap_year : int -> bool
    val nb_days : int -> int -> int
    val string_to_int_cmp : string -> int -> int
    val verify_date : string list -> bool
    val date_is_valid : string -> bool
  end
module Date : DATE
