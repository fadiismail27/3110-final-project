open Game
open Player

(** [print_community_cards cards] prints the list of community cards [cards] to the UI. *)
val print_community_cards : Card.t list -> unit

(** [print_player is_current is_active player_id player] prints the information of [player] to the UI.
  [is_current] indicates if the player is the current player.
  [is_active] indicates if the player is still active in the game.
  [player_id] is the identifier of the player. *)
val print_player : bool -> bool -> int -> Player.t -> unit

(** [print_game_state game round] prints the current state of [game] for the given [round] to the UI. *)
val print_game_state : Game.t -> int -> unit


val print_action_menu : Game.t -> Player.t -> int -> unit
(** Prints the action menu for the current player. [min_bet_to_open] is the
    minimum opening bet (e.g., big blind). *)

(** Represents the possible actions a player can choose. *)
type player_action_choice =
  | Check
  | Bet of int
  | Call
  | Raise of int
  | Fold
  | AllIn

val get_player_action : Game.t -> Player.t -> int -> player_action_choice
(** Gets the player's action choice, including bet/raise amounts if applicable.
    [min_bet_to_open] is the minimum opening bet. *)
