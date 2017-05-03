module type CITIES =
  sig
    val cities_available : string list
    val check_with_city : string -> string list -> bool
    val city_exists : string list -> bool
    val cities_are_valid : string -> bool
  end

module Cities : CITIES =
  struct

  let cities_available = ["Brest"; "Le Havre"; "Lille"; "Paris"; "Strasbourg"; "Nancy"; "Dijon"; "Lyon";
                          "Nice"; "Marseille"; "Montpellier"; "Perpignan"; "Bordeaux";
                          "Nantes"; "Avignon"; "Rennes"; "Biarritz"; "Toulouse"; "Le Mans"]

  let rec check_with_city current cities = match cities with
    | [] -> false
    | head::tail -> if current = head then true else check_with_city current tail

  let rec city_exists city = match city with
    | [] -> true
    | head::tail -> if (check_with_city head cities_available = true && city_exists tail = true) then true else false

  let cities_are_valid cities = city_exists (Str.split (Str.regexp ",") cities);;
end;;
