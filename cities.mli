module type CITIES =
  sig
    type distance = (string * string * int)

    val cities_distance : distance list

    val get_departure : distance -> string
    val get_arrival : distance -> string
    val get_distance : distance -> int

    val cities_available : string list
    val cities_available_for_thalys : string list
    val cities_available_for_Eurostar : string list

    val check_with_city : string -> string list -> bool

    val city_exists : string list -> bool
    val city_exists_for_thalys : string list -> bool
    val city_exists_for_Eurostar : string list -> bool

    val cities_are_valid : string -> bool
    val cities_are_valid_for_thalys : string -> bool
    val cities_are_valid_for_Eurostar : string -> bool

    val get_distance_link : string -> string -> distance list -> int
  end

module Cities : CITIES
