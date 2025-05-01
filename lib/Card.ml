type suit =
  | Hearts
  | Diamonds
  | Clubs
  | Spades

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

let all_ranks =
  [
    Two; Three; Four; Five; Six; Seven; Eight; Nine; Ten; Jack; Queen; King; Ace;
  ]

let all_suits = [ Hearts; Diamonds; Clubs; Spades ]

type t = {
  rank : rank;
  suit : suit;
}

let string_of_card c =
  let rank_str =
    match c.rank with
    | Two -> "2"
    | Three -> "3"
    | Four -> "4"
    | Five -> "5"
    | Six -> "6"
    | Seven -> "7"
    | Eight -> "8"
    | Nine -> "9"
    | Ten -> "10"
    | Jack -> "J"
    | Queen -> "Q"
    | King -> "K"
    | Ace -> "A"
  in
  let suit_str =
    match c.suit with
    | Hearts -> "♥"
    | Diamonds -> "♦"
    | Clubs -> "♣"
    | Spades -> "♠"
  in
  rank_str ^ suit_str

let create_deck () =
  List.concat
    (List.map
       (fun suit -> List.map (fun rank -> { rank; suit }) all_ranks)
       all_suits)

let int_to_rank = function
  | 2 -> Two
  | 3 -> Three
  | 4 -> Four
  | 5 -> Five
  | 6 -> Six
  | 7 -> Seven
  | 8 -> Eight
  | 9 -> Nine
  | 10 -> Ten
  | 11 -> Jack
  | 12 -> Queen
  | 13 -> King
  | 14 -> Ace
  | _ -> failwith "Invalid rank value"

let string_of_rank = function
  | Two -> "2"
  | Three -> "3"
  | Four -> "4"
  | Five -> "5"
  | Six -> "6"
  | Seven -> "7"
  | Eight -> "8"
  | Nine -> "9"
  | Ten -> "10"
  | Jack -> "J"
  | Queen -> "Q"
  | King -> "K"
  | Ace -> "A"
