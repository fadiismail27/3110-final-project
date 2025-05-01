open Card

(** The type representing the rank of a poker hand. *)
type hand_rank =
  | HighCard of Card.t list
  | OnePair of int * Card.t list
  | TwoPair of int * int * Card.t list
  | ThreeOfAKind of int * Card.t list
  | Straight of int
  | Flush of Card.t list
  | FullHouse of int * int
  | FourOfAKind of int * Card.t
  | StraightFlush of int

(** [evaluate_hand cards] returns the best 5-card poker hand from the list [cards].
    Requires: [cards] must contain exactly 7 cards. *)
val evaluate_hand : Card.t list -> hand_rank

(** [best_hands players] takes a list of (name, 7-card hand) pairs and returns
    all players with the strongest evaluated hand. *)
val best_hands : (string * Card.t list) list -> (string * hand_rank) list

(** [string_of_hand_rank hr] returns a human-readable description of [hr]. *)
val string_of_hand_rank : hand_rank -> string

val compare_hands : hand_rank -> hand_rank -> int