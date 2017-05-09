open Train
open Cities
open Hour

module type TRIP =
  sig
    type t
    type trip
    type t_trip

    val create_t : string -> string -> string -> trip
    val create_trip : string -> string -> string -> string list -> trip list
    val print_trip: trip -> unit
    val print_trip_list : trip list -> unit

    val create_whole : t -> trip list -> t_trip
    val get_train : t_trip -> t
    val print_whole : t_trip -> unit
    val print_whole_list : t_trip list -> unit
  end

module type MAKETRIP = functor (Train : TRAIN) -> TRIP with type t = Train.train

module MakeTrip : MAKETRIP = functor (Train : TRAIN) ->
  struct
    type t = Train.train
    type trip = (string * string * string)
    type t_trip = (Train.train * trip list)

    let create_t s a d = (s, a, d);;

    let create_trip date hour train trip =
      let rec aux acc date hour = function
        | [] -> []
        | head::tail when acc = 1 -> let new_t = [create_t head " (,)" ( " (" ^ date ^ "," ^ hour ^ ")" )]
          in List.append new_t (aux (acc+1) date hour tail)
        | head::tail when acc = List.length trip -> let new_t = [create_t head
          ( " (" ^ date ^ "," ^ (Hour.calculate_hour_with_distance hour (Cities.get_distance_link (List.nth trip (acc-2)) head Cities.cities_distance) train) ^ ")" )
          ( " (,)" )]
          in List.append new_t (aux (acc+1) date hour tail)
        | head::tail -> let new_t = [create_t head
        ( " (" ^ date ^ "," ^ (Hour.calculate_hour_with_distance hour (Cities.get_distance_link (List.nth trip (acc-2)) head Cities.cities_distance) train) ^ ")" )
        ( " (" ^ date ^ "," ^ (Hour.calculate_hour ((Str.split (Str.regexp ":") (Hour.calculate_hour_with_distance hour ((Cities.get_distance_link (List.nth trip (acc-2)) head Cities.cities_distance)) train ))) 10) ^ ")" )]
          in List.append new_t (aux (acc+1) date (Hour.calculate_hour ((Str.split (Str.regexp ":") (Hour.calculate_hour_with_distance hour ((Cities.get_distance_link (List.nth trip (acc-2)) head Cities.cities_distance)) train))) 10) tail)
      in aux 1 date hour trip

    let print_trip (s, a, d) = print_string s; print_string a; print_endline d;;

    let rec print_trip_list = function
      | [] -> ()
      | head::tail -> print_trip head ; print_trip_list tail;;

    let create_whole train trip = (train, trip);;

    let get_train (train, _) = train;;

    let print_whole (train, trip) = Train.print_train train ; print_trip_list trip;;

    let rec print_whole_list = function
      | [] -> ()
      | head::tail -> print_whole head ; print_whole_list tail
  end;;
