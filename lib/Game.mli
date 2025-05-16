type t = {
  players : Player.t list;
  deck : Card.t list;
  pot : int;
  community_cards : Card.t list;
  current_bet : int;
}

val get_players : t -> Player.t list
(** [get_players t] returns the list of players currently in the game [t]. *)

val get_pot : t -> int
(** [get_pot t] returns the current value of the pot in the game state [t]. *)

val get_current_bet : t -> int
(** [get_current_bet t] returns the current highest bet placed in the ongoing
    round of game [t]. *)

val get_community_cards : t -> Card.t list
(** [get_community_cards t] returns the list of community cards currently on the
    table in the game state [t]. *)

val get_deck : t -> Card.t list
(** [get_deck gs] returns the deck of cards in the game state. *)

val create_game : string list -> int -> t
(** [create_game players big_blind] initializes a new game state with the given
    players and sets the big blind amount. The deck is shuffled and the pot is
    initialized to 0. *)

(* val get_player_by_id : t -> int -> Player.t option
(** [get_player_by_id gs player_id] returns the player with the given ID from
    the game state. *) *)

val deal_initial_hands :
  Player.t list -> Card.t list -> Player.t list * Card.t list
(** [deal_initial_hands players deck] deals two cards to each player from the
    deck. Returns a tuple of updated players and remaining deck. *)

val shuffle_deck : t -> t
(** [shuffle_deck gs] returns a new game state with the deck shuffled. *)

val is_game_over : t -> bool (* Checks if only one player > 0 chips *)
(** [is_game_over gs] checks if only one player has chips left. *)

val new_hand : t -> t
(** [new_hand gs] prepares the game state for a new hand: Resets relevant player
    state, collects cards, shuffles deck, deals new hands. *)

val reveal_community_cards : t -> int -> t
(** [reveal_community_cards gs num_cards] reveals the next [num_cards] from the
    deck to the community cards and returns the updated game state. *)

(* --- Player Action Handlers --- *)

val handle_player_fold : t -> int -> t
(** [handle_player_fold gs player_id] updates game state for player folding. *)

val handle_player_check : t -> int -> t
(** [handle_player_check gs player_id] updates game state for player checking.
*)

val handle_player_bet : t -> int -> int -> t
(** [handle_player_bet gs player_id bet_amount] updates game state for player
    betting. *)

val handle_player_call : t -> int -> t
(** [handle_player_call gs player_id] updates game state for player calling. *)

val handle_player_raise : t -> int -> int -> t
(** [handle_player_raise gs player_id new_total_bet] updates game state for
    player raising. *)

val distribute_pot : t -> int list -> t
(** [distribute_pot gs winner_ids] distributes the main pot to the players
    identified by [winner_ids], updating their chips and resetting the pot. *)
