type t = {
  players : Player.t list;
  deck : Card.t list;
  pot : int;           
  community_cards : Card.t list;
  current_bet : int;    
}

val get_players : t -> Player.t list
val get_pot : t -> int
val get_current_bet : t -> int
val get_community_cards : t -> Card.t list
val get_deck : t -> Card.t list
val create_game : string list -> int -> t
val deal_initial_hands : Player.t list -> Card.t list -> Player.t list * Card.t list

(** [shuffle_deck gs] returns a new game state with the deck shuffled. *)
val shuffle_deck : t -> t

(** [is_game_over gs] checks if only one player has chips left. *)
val is_game_over : t -> bool (* Checks if only one player > 0 chips *)

(** [new_hand gs] prepares the game state for a new hand:
    Resets relevant player state, collects cards, shuffles deck, deals new hands. *)
val new_hand : t -> t

(** [reveal_community_cards gs num_cards] reveals the next [num_cards] from the deck
    to the community cards and returns the updated game state. *)
val reveal_community_cards : t -> int -> t
