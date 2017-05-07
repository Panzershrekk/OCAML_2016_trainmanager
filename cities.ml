module type CITIES =
  sig
    type distance = (string * string * int)

    val cities_distance : distance list

    val get_departure : distance -> string
    val get_arrival : distance -> string
    val get_distance : distance -> int

    val cities_available : string list
    val check_with_city : string -> string list -> bool
    val city_exists : string list -> bool
    val cities_are_valid : string -> bool
    val get_distance_link : string -> string -> distance list -> int
  end

module Cities : CITIES =
  struct

  type distance = (string * string * int);;

  let cities_distance = [
                        ("Brest", "Rennes", 248);
                        ("Rennes", "Le Mans", 163);
                        ("Le Mans", "Nantes", 183);
                        ("Le Mans", "Paris", 201);
                        ("Paris","Bordeaux",568);
                        ("Lille", "Brussels", 106);
                        ("Lille","London",269);
                        ("Paris", "Lille", 225);
                        ("Brussels","Liege",104);
                        ("Paris", "Le Havre",230);
                        ("Liege","Cologne", 118);
                        ("Cologne","Essen",81);
                        ("Paris", "Nancy",327);
                        ("Dijon","Nancy", 226);
                        ("Nancy","Strasbourg",149);
                        ("Paris","Strasbourg",449);
                        ("Brussels","Amsterdam",211);
                        ("Dijon","Strasbourg",309)
                        ]

  let get_departure (d, _, _) = d
  let get_arrival (_, a, _) = a
  let get_distance (_, _, dis) = dis

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

  let rec get_distance_link city_1 city_2 = function
    | [] -> 0
    | head::tail -> if (city_1 = (get_departure head) && city_2 = (get_arrival head)) then get_distance head else get_distance_link city_1 city_2 tail
end;;
