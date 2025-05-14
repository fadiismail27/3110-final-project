(** Simulates the stages of a Texas Hold'em hand (pre-flop, flop, turn, river)
    including betting rounds.
    Takes the current game state, the dealer button position (int index),
    and the big blind value (int). Returns the updated game state. *)
val play_hand_stages : Game.t -> int -> int -> Game.t