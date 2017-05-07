open Train

module type TRIP =
  sig
    type t
    type trip
    type t_trip

    val create_t : string -> string -> string -> trip
    val create_trip : string -> string -> string list -> trip list
    val print_trip: trip -> unit
  end

module type MAKETRIP = functor (Train : TRAIN) -> TRIP with type t = Train.train

module MakeTrip : MAKETRIP = functor (Train : TRAIN) ->
  struct
    type t = Train.train
    type trip = (string * string * string)
    type t_trip = (Train.train * trip)

    let create_t s a d = (s, a, d);;

    let create_trip date hour trip =
      let rec aux acc = function
        | [] -> []
        | head::tail when acc = 1 -> let new_t = [create_t head "(,)" ( "(" ^ date ^ "," ^ hour ^ ")" )]
          in List.append new_t (aux (acc+1) tail)
        | head::tail when acc = List.length trip -> let new_t = [create_t head "(,)" ( "(" ^ date ^ "," ^ hour ^ ")" )]
          in List.append new_t (aux (acc+1) tail)
        | head::tail -> let new_t = [create_t head "(,)" ( "(" ^ date ^ "," ^ hour ^ ")" )]
          in List.append new_t (aux (acc+1) tail)
      in aux 1 trip
    (*let create_trip trip_list date hour trip =
      let rec aux acc = function
        | [] -> trip_list
        | head::tail when acc = 0 -> create head "(,)" ("(" ^ date ^ "," ^ hour ^ ")")
        | head::tail when acc = List.length trip -> create head "(,)" ("(" ^ date ^ "," ^ hour ^ ")")
        | head::tail -> create head "(,)" ("(" ^ date ^ "," ^ hour ^ ")")
      in aux 0 trip;;*)

    let print_trip (s, a, d) = print_string s; print_string a; print_string d;;
  end;;
