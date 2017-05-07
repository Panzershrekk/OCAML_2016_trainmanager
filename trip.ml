open Train
open Cities
open Hour

module type TRIP =
  sig
    type t
    type trip
    type t_trip

    val create_t : string -> string -> string -> trip
    val create_trip : string -> string -> string list -> trip list
    val print_trip: trip -> unit
    val print_trip_list : trip list -> unit
  end

module type MAKETRIP = functor (Train : TRAIN) -> TRIP with type t = Train.train

module MakeTrip : MAKETRIP = functor (Train : TRAIN) ->
  struct
    type t = Train.train
    type trip = (string * string * string)
    type t_trip = (Train.train * trip)

    let create_t s a d = (s, a, d);;

    let create_trip date hour trip =
      let rec aux acc date hour = function
        | [] -> []
        | head::tail when acc = 1 -> let new_t = [create_t head " (,)" ( " (" ^ date ^ "," ^ hour ^ ")" )]
          in List.append new_t (aux (acc+1) date hour tail)
        | head::tail when acc = List.length trip -> let new_t = [create_t head
          ( " (" ^ date ^ "," ^ (Hour.calculate_hour_with_distance hour (Cities.get_distance_link (List.nth trip (acc-2)) head Cities.cities_distance)) ^ ")" )
          ( " (,)" )]
          in List.append new_t (aux (acc+1) date hour tail)
        | head::tail -> let new_t = [create_t head
        ( " (" ^ date ^ "," ^ (Hour.calculate_hour_with_distance hour (Cities.get_distance_link (List.nth trip (acc-2)) head Cities.cities_distance)) ^ ")" )
        ( " (" ^ date ^ "," ^ (Hour.calculate_hour ((Str.split (Str.regexp ":") (Hour.calculate_hour_with_distance hour ((Cities.get_distance_link (List.nth trip (acc-2)) head Cities.cities_distance))))) 10) ^ ")" )]
          in List.append new_t (aux (acc+1) date (Hour.calculate_hour ((Str.split (Str.regexp ":") (Hour.calculate_hour_with_distance hour ((Cities.get_distance_link (List.nth trip (acc-2)) head Cities.cities_distance))))) 10) tail)
      in aux 1 date hour trip

    let print_trip (s, a, d) = print_string s; print_string a; print_endline d;;

    let rec print_trip_list = function
      | [] -> ()
      | head::tail -> print_trip head ; print_trip_list tail
  end;;
