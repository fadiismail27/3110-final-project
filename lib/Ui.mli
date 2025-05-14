open Game
open Player

val print_community_cards : Card.t list -> unit
val print_player : bool -> bool -> int -> Player.t -> unit
val print_game_state : Game.t -> int -> unit

(** Prints the action menu for the current player. 
    [min_bet_to_open] is the minimum opening bet (e.g., big blind). *)
val print_action_menu : Game.t -> Player.t -> int -> unit

(** Represents the possible actions a player can choose. *)
type player_action_choice =
  | Check
  | Bet of int
  | Call
  | Raise of int
  | Fold
  | AllIn

(** Gets the player's action choice, including bet/raise amounts if applicable. 
    [min_bet_to_open] is the minimum opening bet. *)
val get_player_action : Game.t -> Player.t -> int -> player_action_choice
