module type TRAIN=
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
