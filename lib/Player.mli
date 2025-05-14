type t = {
  id : int;
  name : string;
  chips : int;
  hand : Card.t list;
  folded : bool;
  current_bet : int;
  is_all_in : bool;
}

val get_id : t -> int
val get_name : t -> string
val get_chips : t -> int
val get_hand : t -> Card.t list
val get_current_bet : t -> int
val is_folded : t -> bool
val create_player : int -> string -> int -> t
val reset_for_new_hand : t -> t
val set_folded : t -> bool -> t
val update_chips : t -> int -> t
val is_all_in : t -> bool
val set_current_bet : t -> int -> t
val set_all_in : t -> bool -> t


(** [fold p] returns a new player state where the player has folded *)
val fold : t -> t

(** [bet p amount] adds [amount] to the player's current bet for the round.
    Updates chips and all-in status if necessary.
    Raises Invalid_argument if bet amount is negative. *)
val bet : t -> int -> t

(** [call p game_current_bet] updates player to match the [game_current_bet].
    Updates chips, player's current_bet, and all-in status if necessary. *)
val call : t -> int -> t

(** [check p game_current_bet] allows player to pass if no bet is due.
    Raises Invalid_argument if there's an outstanding bet to call. *)
val check : t -> int -> t

(** [raise_bet p new_total_bet] updates player to raise the bet to [new_total_bet] for the round.
    Raises Invalid_argument if [new_total_bet] is not greater than player's current contribution.
    Updates chips, player's current_bet, and all-in status. *)
val raise_bet : t -> int -> t

(** [is_folded p] returns true if the player has folded *)
val is_folded : t -> bool

(** [get_chips p] returns the number of chips the player has *)
val get_chips : t -> int

(** [get_hand p] returns the player's current hand *)
val get_hand : t -> Card.t list

