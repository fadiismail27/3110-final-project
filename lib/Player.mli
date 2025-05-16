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
(** [get_id player] returns the unique identifier of the player [player]. *)

val get_name : t -> string
(** [get_name player] returns the name of the given [player] as a string. *)

val get_chips : t -> int
(** [get_chips player] returns the number of chips currently held by [player].
*)

val get_hand : t -> Card.t list
(** [get_hand player] returns the list of cards currently held by [player]. *)

val get_current_bet : t -> int
(** [get_current_bet t] returns the current bet amount placed by the player [t].
*)

val is_folded : t -> bool
(** [is_folded t] returns [true] if the player [t] has folded in the current
    round, and [false] otherwise. *)

val create_player : int -> string -> int -> t
(** [create_player id name score] creates a new player with the given [id],
    [name], and [score]. *)

val reset_for_new_hand : t -> t
(** [reset_for_new_hand player] returns a new player record with fields reset as
    appropriate for the start of a new hand (e.g., clearing the player's current
    hand, resetting bets, etc.), while preserving persistent player information
    such as name and total chips. *)

val set_folded : t -> bool -> t
(** [set_folded player folded] returns a new player record with the [folded]
    status set to [folded]. This does not mutate the original player, but
    produces an updated copy. *)

val update_chips : t -> int -> t
(** [update_chips player amount] returns a new player record with the chip count
    updated by [amount]. Positive [amount] increases the player's chips, while
    negative [amount] decreases them. *)

val is_all_in : t -> bool
(** [is_all_in player] returns [true] if the player [player] has bet all of
    their chips (i.e., is "all in"), and [false] otherwise. *)

val set_current_bet : t -> int -> t
(** [set_current_bet player amount] returns a new player record with the current
    bet set to [amount]. This function does not mutate the original player, but
    instead returns an updated copy. *)

val set_all_in : t -> bool -> t
(** [set_all_in player status] returns a new player record with the [all_in]
    status set to [status]. This function does not mutate the original player
    but returns an updated copy. *)

val fold : t -> t
(** [fold p] returns a new player state where the player has folded *)

val bet : t -> int -> t
(** [bet p amount] adds [amount] to the player's current bet for the round.
    Updates chips and all-in status if necessary. Raises Invalid_argument if bet
    amount is negative. *)

val call : t -> int -> t
(** [call p game_current_bet] updates player to match the [game_current_bet].
    Updates chips, player's current_bet, and all-in status if necessary. *)

val check : t -> int -> t
(** [check p game_current_bet] allows player to pass if no bet is due. Raises
    Invalid_argument if there's an outstanding bet to call. *)

val raise_bet : t -> int -> t
(** [raise_bet p new_total_bet] updates player to raise the bet to
    [new_total_bet] for the round. Raises Invalid_argument if [new_total_bet] is
    not greater than player's current contribution. Updates chips, player's
    current_bet, and all-in status. *)

val is_folded : t -> bool
(** [is_folded p] returns true if the player has folded *)

val get_chips : t -> int
(** [get_chips p] returns the number of chips the player has *)

val get_hand : t -> Card.t list
(** [get_hand p] returns the player's current hand *)
