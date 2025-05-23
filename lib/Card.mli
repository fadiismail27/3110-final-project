type rank =
  | Two | Three | Four | Five | Six | Seven | Eight
  | Nine | Ten | Jack | Queen | King | Ace

type suit = Hearts | Diamonds | Clubs | Spades


type t = {
  rank : rank;
  suit : suit;
}

(** [String_of_card] turns the card [t] into a string*)
val string_of_card : t -> string
(** [create_deck] initializes a list of cards*)
val create_deck : unit -> t list
val int_to_rank : int -> rank
val string_of_rank : rank -> string

(** [all_ranks] is a list of all possible card ranks *)
val all_ranks : rank list

(** [all_suits] is a list of all possible card suits *)
val all_suits : suit list
