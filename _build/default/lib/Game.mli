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

val deal_initial_hands :
  Player.t list -> Card.t list -> Player.t list * Card.t list
