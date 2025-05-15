(** Simulates the stages of a Texas Hold'em hand (pre-flop, flop, turn, river)
    including betting rounds.
    Takes the current game state, the dealer button position (int index),
    and the big blind value (int). Returns the updated game state. *)
val play_hand_stages : Game.t -> int -> int -> Game.t
val reset_folds : Player.t list -> Player.t list
val reset_player_bets_for_street : Game.t -> Game.t
val get_starting_player_index : Player.t list -> int -> int
val handle_bot_action : Game.t -> Player.t -> int -> Game.t * int option * bool
