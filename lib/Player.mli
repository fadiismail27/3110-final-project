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

(** [fold p] returns a new player state where the player has folded *)
val fold : t -> t

(** [bet p amount] returns a new player state after betting the given amount.
    Raises Invalid_argument if player has insufficient chips *)
val bet : t -> int -> t

(** [is_folded p] returns true if the player has folded *)
val is_folded : t -> bool

(** [get_chips p] returns the number of chips the player has *)
val get_chips : t -> int

(** [get_hand p] returns the player's current hand *)
val get_hand : t -> Card.t list

