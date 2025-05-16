type rank =
  | Two
  | Three
  | Four
  | Five
  | Six
  | Seven
  | Eight
  | Nine
  | Ten
  | Jack
  | Queen
  | King
  | Ace

type suit =
  | Hearts
  | Diamonds
  | Clubs
  | Spades

type t = {
  rank : rank;
  suit : suit;
}

val string_of_card : t -> string
(** [string_of_card c] returns a string representation of the card [c]. *)

val create_deck : unit -> t list
(** [create_deck ()] returns a list of all cards in a standard deck. *)

val int_to_rank : int -> rank
(** [int_to_rank i] converts an integer [i] to its corresponding rank. Raises:
    [Invalid_argument] if [i] is not a valid rank index. *)

val string_of_rank : rank -> string
(** [string_of_rank r] returns the string representation of the rank [r]. *)

val all_ranks : rank list
(** [all_ranks] is the list of all possible ranks in order. *)

val all_suits : suit list
(** [all_suits] is the list of all possible suits. *)
