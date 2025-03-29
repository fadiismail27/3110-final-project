open OUnit2
open Final_project
open Card

let test_addition _ = assert_equal (1 + 1) 2

(* Add more tests here *)
(* test card simulation *)
let test_card_simulation _ =
  let deck = Card.create_deck () in
  assert_equal (List.length deck) 52;
  assert_equal
    (List.length (List.filter (fun c -> c.suit = Card.Hearts) deck))
    13;
  assert_equal
    (List.length (List.filter (fun c -> c.suit = Card.Diamonds) deck))
    13;
  assert_equal
    (List.length (List.filter (fun c -> c.suit = Card.Clubs) deck))
    13;
  assert_equal
    (List.length (List.filter (fun c -> c.suit = Card.Spades) deck))
    13;
  assert_equal (List.length (List.filter (fun c -> c.rank = Card.Ace) deck)) 4
