(** Simulates the stages of a Texas Hold'em hand (pre-flop, flop, turn, river)
    including betting rounds. Takes the current game state, the dealer button
    position (int index), and the big blind value (int). Returns the updated
    game state. *)
val play_hand_stages : Game.t -> int -> int -> Game.t
(** [play_hand_stages game start_idx num_players] progresses the game through
    the stages of a hand, starting from player at [start_idx] with [num_players]
    players. Returns the updated game state. *)

val reset_folds : Player.t list -> Player.t list
(** [reset_folds players] resets the fold status for all players in the list
    [players]. Returns the updated list of players. *)

val reset_player_bets_for_street : Game.t -> Game.t
(** [reset_player_bets_for_street game] resets the bets for all players in the
    current street of [game]. Returns the updated game state. *)

val get_starting_player_index : Player.t list -> int -> int
(** [get_starting_player_index players dealer_idx] returns the index of the
    player who should start the action, given the list of [players] and the
    [dealer_idx]. *)

val handle_bot_action : Game.t -> Player.t -> int -> Game.t * int option * bool
(** [handle_bot_action game bot_player min_bet] processes the action for a bot
    player [bot_player] in [game], given the current [min_bet]. Returns a tuple
    of the updated game state, an optional bet amount, and a boolean indicating
    if the bot folded. *)
