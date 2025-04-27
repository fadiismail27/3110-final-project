open OUnit2
open Final_project
open Card
open Player
open Game
open Round

(*--------------------------------------------------------------------*)
(*  Helper functions                                                   *)
(*--------------------------------------------------------------------*)
(* Create a list of players with given names and chips *)
let mk_players names chips =
  List.mapi (fun idx n -> Player.create_player idx n chips) names

let mk_state ?(chips = 1000) names =
  let gs = Game.create_game names chips in
  let players, deck =
    Game.deal_initial_hands (Game.get_players gs) (Game.get_deck gs)
  in
  { gs with players; deck }

(*--------------------------------------------------------------------*)
(*  Card tests                                                         *)
(*--------------------------------------------------------------------*)

let test_deck_size _ =
  let deck = Card.create_deck () in
  assert_equal 52 (List.length deck) ~msg:"A standard deck should have 52 cards"

let test_deck_uniqueness _ =
  let deck = Card.create_deck () in
  let uniq = List.sort_uniq compare deck in
  assert_equal 52 (List.length uniq) ~msg:"Deck should not contain duplicates"

(*--------------------------------------------------------------------*)
(*  Player tests                                                       *)
(*--------------------------------------------------------------------*)

let test_negative_chips _ =
  assert_raises
    (Invalid_argument "Chips cannot be negative")
    (fun () -> Player.create_player 0 "Bad" (-10))

let test_accessor_roundtrip _ =
  let p = Player.create_player 1 "Bob" 500 in
  assert_bool "get_name" (Player.get_name p = "Bob");
  assert_bool "get_chips" (Player.get_chips p = 500);
  assert_bool "initially not folded" (not (Player.is_folded p))

(*--------------------------------------------------------------------*)
(*  Game tests                                                         *)
(*--------------------------------------------------------------------*)

let test_create_game_invalid_chip _ =
  assert_raises (Invalid_argument "Initial chips must be positive") (fun () -> Game.create_game [ "A"; "B" ] 0)

let test_create_game_insufficient_players _ =
  assert_raises
    (Invalid_argument "Need at least 2 players")
    (fun () -> Game.create_game [ "Solo" ] 1000)

let test_deal_initial_hands _ =
  let names = [ "A"; "B"; "C" ] in
  let gs = Game.create_game names 1000 in
  let players, deck' =
    Game.deal_initial_hands (Game.get_players gs) (Game.get_deck gs)
  in
  (* Every player should now have exactly two cards *)
  List.iter (fun p -> assert_equal 2 (List.length (Player.get_hand p))) players;
  (* Deck should have shrunk appropriately *)
  let cards_removed = 2 * List.length names in
  assert_equal (52 - cards_removed) (List.length deck')

(*--------------------------------------------------------------------*)
(*  Round tests  (avoid user input by excluding "You")                 *)
(*--------------------------------------------------------------------*)

let test_play_round_changes_chips _ =
  Random.init 0;
  (* deterministic *)
  let names = [ "CPU 1"; "CPU 2"; "CPU 3" ] in
  let state0 = mk_state names in
  let total_chips_before =
    List.fold_left
      (fun acc p -> acc + Player.get_chips p)
      0 (Game.get_players state0)
  in
  let state1 = Round.play_round state0 in
  let total_chips_after =
    List.fold_left
      (fun acc p -> acc + Player.get_chips p)
      0 (Game.get_players state1)
  in
  (* Exactly 100 chips are injected each round by our simple logic *)
  assert_equal (total_chips_before + 100) total_chips_after;
  (* Exactly one player should have received the 100‑chip bonus *)
  let bonuses =
    List.map2
      (fun p0 p1 -> Player.get_chips p1 - Player.get_chips p0)
      (Game.get_players state0) (Game.get_players state1)
  in
  assert_equal 1 (List.length (List.filter (( <> ) 0) bonuses))

(*--------------------------------------------------------------------*)
(*  Test suite aggregation                                             *)
(*--------------------------------------------------------------------*)

let suite =
  "OCaml Poker – Unit Tests"
  >::: [
         "deck size" >:: test_deck_size;
         "deck uniqueness" >:: test_deck_uniqueness;
         "negative chips" >:: test_negative_chips;
         "player accessors" >:: test_accessor_roundtrip;
         "create_game invalid chips" >:: test_create_game_invalid_chip;
         "create_game single player" >:: test_create_game_insufficient_players;
         "deal_initial_hands" >:: test_deal_initial_hands;
         "play_round chip changes" >:: test_play_round_changes_chips;
       ]

let () = run_test_tt_main suite
