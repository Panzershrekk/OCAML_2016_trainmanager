module type TRAIN =
  sig
    type train = (string * string)
    val random_between : int -> int -> int
    val create_train : string -> train

    val get_speed : train -> int
    val get_type : train -> string
    val get_id : train -> string

    val print_train : train -> unit
    val print_all_train : train list -> unit
  end

module Train : TRAIN =
  struct
      type train = (string * string)


      let get_speed (t, _) = if t = "TGV" then 230 else
                             if t = "Thalys" then 210 else
                             if t = "Eurostar" then 160 else
                             0

      let get_type (t, _) = t

      let get_id (_, i) = i

      let print_train (t, i) = print_endline (t ^ " " ^ i)

      let random_between first_born snd_born = (Random.int (snd_born - first_born) + first_born)

      let create_train type_train = let train = (type_train, string_of_int (random_between 1000 10000)) in ( print_string "Trip created: " ; print_train train ; train)

      let rec print_all_train = function
        | [] -> ()
        | head::tail -> print_train head ; print_all_train tail

  end;;
