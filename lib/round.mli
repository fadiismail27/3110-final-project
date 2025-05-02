(** Simulates the stages of a hand (deal, flop, turn, river) with pauses. *)
val play_hand_stages : Game.t -> Game.t
val play_round : Game.t -> Game.t