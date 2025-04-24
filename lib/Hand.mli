(** Type representing the strength of a poker hand. *)
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

(** [evaluate_hand cards] takes 7 cards and returns the best 5-card hand rank. *)
val evaluate_hand : Card.t list -> hand_rank

(** [compare_hands h1 h2] compares two poker hands. Returns:
    - 1 if h1 is better
    - -1 if h2 is better
    - 0 if they are equal *)
val compare_hands : hand_rank -> hand_rank -> int

(** [best_hands players] takes a list of (name, 7-card hand) pairs and returns
    all players who have the strongest evaluated hand. *)
val best_hands : (string * Card.t list) list -> (string * hand_rank) list

(** [string_of_hand_rank h] returns a human-readable description of [h]. *)
val string_of_hand_rank : hand_rank -> string
