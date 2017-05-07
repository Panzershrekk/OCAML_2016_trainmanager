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

module Train : TRAIN
