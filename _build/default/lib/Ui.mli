val print_community_cards : Card.t list -> unit
val print_player : bool -> bool -> int -> Player.t -> unit
val print_game_state : Game.t -> int -> unit
val print_action_menu : Game.t -> Player.t -> int -> unit
val get_player_action : unit -> [ `Call | `Raise | `Fold | `AllIn ]
