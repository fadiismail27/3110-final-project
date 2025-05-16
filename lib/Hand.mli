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

val evaluate_hand : Card.t list -> hand_rank
(** [evaluate_hand cards] returns the best 5-card poker hand from the list
    [cards]. Requires: [cards] must contain exactly 7 cards. *)

val best_hands : (string * Card.t list) list -> (string * hand_rank) list
(** [best_hands players] takes a list of (name, 7-card hand) pairs and returns
    all players with the strongest evaluated hand. *)

val string_of_hand_rank : hand_rank -> string
(** [string_of_hand_rank hr] returns a human-readable description of [hr]. *)

val compare_hands : hand_rank -> hand_rank -> int
(** [compare_hands h1 h2] compares two hand ranks [h1] and [h2]. Returns a
    negative integer if [h1] is ranked lower than [h2], zero if they are equal,
    and a positive integer if [h1] is ranked higher than [h2]. *)
