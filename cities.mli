module type CITIES =
  sig
    val cities_available : string list
    val check_with_city : string -> string list -> bool
    val city_exists : string list -> bool
    val cities_are_valid : string -> bool
  end

module Cities : CITIES
