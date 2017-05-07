module type TRAIN =
  sig
    type train = (string * string)
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

      let create_train type_train = (type_train, string_of_int (Random.int 10000))

      let get_speed (t, _) = if t = "TGV" then 230 else
                             if t = "Thalys" then 210 else
                             if t = "Eurostar" then 160 else
                             0

      let get_type (t, _) = t

      let get_id (_, i) = i

      let print_train (t, i) = print_endline (t ^ " " ^ i)

      let rec print_all_train = function
        | [] -> ()
        | head::tail -> print_train head ; print_all_train tail

  end;;
(*module type TRAIN=
  sig
    type name
    type t = name

    val get_speed : int
  end;;

module type TYPETRAIN =
  sig
  	type t

    val get_train_speed : int
  	end;;

module type MYTRAIN = functor (Train : TRAIN) -> TYPETRAIN with type t = Train.t;;

module TgvTrain =
  struct
    type name = string
    type t = name
    let t = "TGV"

    let get_speed = 230
end;;

module ThalysTrain =
  struct
    type name = string
    type t = name
    let t = "Thalys"

    let get_speed = 210
end;;

module EuroTrain =
  struct
    type name = string
    type t = name
    let t = "Eurostar"

    let get_speed = 160
end;;

module MyTrain : MYTRAIN = functor (Train : TRAIN) ->
	struct
		type t = Train.t

    let get_train_speed  = Train.get_speed
	end;;
*)
