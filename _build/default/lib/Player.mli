type t = {
  id : int;
  name : string;
  chips : int;
  hand : Card.t list;
  folded : bool;
  current_bet : int;
  is_all_in : bool;
}

val get_name : t -> string
val get_chips : t -> int
val is_folded : t -> bool
val get_hand : t -> Card.t list
val is_all_in : t -> bool
val get_current_bet : t -> int
val create_player : int -> string -> int -> t