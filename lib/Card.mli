type rank =
  | Two | Three | Four | Five | Six | Seven | Eight
  | Nine | Ten | Jack | Queen | King | Ace

type suit = Hearts | Diamonds | Clubs | Spades


type t = {
  rank : rank;
  suit : suit;
}


val string_of_card : t -> string
val create_deck : unit -> t list
