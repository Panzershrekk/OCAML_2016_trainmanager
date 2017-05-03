module type HOUR =
  sig
    val string_to_int_cmp : string -> int -> int
    val verify_hour : string list -> bool
    val hour_is_valid : string -> bool
  end
module Hour : HOUR
