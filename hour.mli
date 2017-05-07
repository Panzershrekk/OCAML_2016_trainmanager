module type HOUR =
  sig
    val string_to_int_cmp : string -> int -> int
    val verify_hour : string list -> bool
    val hour_is_valid : string -> bool
    val add_minute : int -> string
    val add_hour : int -> string
    val round_value : float -> float
    val calculate_hour : string list -> int -> string
    val calculate_hour_with_distance : string -> int -> string
  end
module Hour : HOUR
