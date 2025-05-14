open OUnit2
open Final_project
open Card
open Player
open Game
open Round
open Final_project.Hand

(*------------------------ Helper functions ------------------------*)

let make_card r s = { rank = r; suit = s }

let mk_players names chips =
  List.mapi (fun idx n -> Player.create_player idx n chips) names

let mk_state ?(chips = 1000) names =
  let gs = Game.create_game names chips in
  let players, deck =
    Game.deal_initial_hands (Game.get_players gs) (Game.get_deck gs)
  in
  { gs with players; deck }

(*------------------------ Card Tests ------------------------*)

let test_deck_size _ =
  let deck = Card.create_deck () in
  assert_equal 52 (List.length deck)

let test_deck_uniqueness _ =
  let deck = Card.create_deck () in
  let uniq = List.sort_uniq compare deck in
  assert_equal 52 (List.length uniq)

let test_card_creation _ =
  let card = { rank = Ace; suit = Spades } in
  assert_equal Ace card.rank;
  assert_equal Spades card.suit

let test_all_ranks _ =
  assert_equal 13 (List.length all_ranks);
  assert_bool "Contains Ace" (List.mem Ace all_ranks);
  assert_bool "Contains Two" (List.mem Two all_ranks)

let test_all_suits _ =
  assert_equal 4 (List.length all_suits);
  assert_bool "Contains Hearts" (List.mem Hearts all_suits)

let test_string_of_card _ =
  assert_equal "A♠" (string_of_card { rank = Ace; suit = Spades });
  assert_equal "K♥" (string_of_card { rank = King; suit = Hearts });
  assert_equal "10♣" (string_of_card { rank = Ten; suit = Clubs })

let test_create_deck _ =
  let deck = create_deck () in
  assert_equal 52 (List.length deck);
  (* Test for specific cards *)
  assert_bool "Contains Ace of Spades" 
    (List.exists (fun c -> c.rank = Ace && c.suit = Spades) deck)

let test_card_basics _ =
  (* Test card creation *)
  let card = { rank = Ace; suit = Spades } in
  assert_equal Ace card.rank;
  assert_equal Spades card.suit;
  assert_equal "A♠" (string_of_card card)

let test_deck_creation _ =
  let deck = create_deck () in
  assert_equal 52 (List.length deck);
  (* Verify we have all combinations *)
  List.iter 
    (fun suit -> 
      List.iter 
        (fun rank ->
          assert_bool 
            (Printf.sprintf "Missing %s" (string_of_card {rank; suit}))
            (List.exists (fun c -> c.rank = rank && c.suit = suit) deck))
        all_ranks)
    all_suits

let test_int_to_rank _ =
  assert_equal Two (int_to_rank 2);
  assert_equal Three (int_to_rank 3);
  assert_equal Four (int_to_rank 4);
  assert_equal Five (int_to_rank 5);
  assert_equal Six (int_to_rank 6);
  assert_equal Seven (int_to_rank 7);
  assert_equal Eight (int_to_rank 8);
  assert_equal Nine (int_to_rank 9);
  assert_equal Ten (int_to_rank 10);
  assert_equal Jack (int_to_rank 11);
  assert_equal Queen (int_to_rank 12);
  assert_equal King (int_to_rank 13);
  assert_equal Ace (int_to_rank 14);
  assert_raises (Failure "Invalid rank value") 
    (fun () -> int_to_rank 1);
  assert_raises (Failure "Invalid rank value") 
    (fun () -> int_to_rank 15)

let test_string_of_rank _ =
  assert_equal "2" (string_of_rank Two);
  assert_equal "3" (string_of_rank Three);
  assert_equal "4" (string_of_rank Four);
  assert_equal "5" (string_of_rank Five);
  assert_equal "6" (string_of_rank Six);
  assert_equal "7" (string_of_rank Seven);
  assert_equal "8" (string_of_rank Eight);
  assert_equal "9" (string_of_rank Nine);
  assert_equal "10" (string_of_rank Ten);
  assert_equal "J" (string_of_rank Jack);
  assert_equal "Q" (string_of_rank Queen);
  assert_equal "K" (string_of_rank King);
  assert_equal "A" (string_of_rank Ace)

(*------------------------ Player Tests ------------------------*)

let test_negative_chips _ =
  assert_raises (Invalid_argument "Chips cannot be negative") (fun () ->
      Player.create_player 0 "Bad" (-10))

let test_accessor_roundtrip _ =
  let p = Player.create_player 1 "Bob" 500 in
  assert_equal "Bob" (Player.get_name p);
  assert_equal 500 (Player.get_chips p);
  assert_bool "initially not folded" (not (Player.is_folded p))

let test_player_actions _ =
  let player = Player.create_player 0 "TestPlayer" 1000 in
  (* Test folding *)
  let player_folded = Player.fold player in
  assert_bool "Player should be folded" (Player.is_folded player_folded);
  assert_bool "Original player should not be folded" (not (Player.is_folded player));

  (* Test betting *)
  let player_bet = Player.bet player 500 in
  assert_equal 500 (Player.get_chips player_bet);
  assert_equal 1000 (Player.get_chips player); (* Original player unchanged *)
  
  (* Test invalid bet *)
  assert_raises (Invalid_argument "Insufficient chips")
    (fun () -> Player.bet player 2000)

(*------------------------ Game Tests ------------------------*)

let test_create_game_invalid_chip _ =
  assert_raises (Invalid_argument "Initial chips must be positive") (fun () ->
      Game.create_game [ "A"; "B" ] 0)

let test_create_game_insufficient_players _ =
  assert_raises (Invalid_argument "Need at least 2 players") (fun () ->
      Game.create_game [ "Solo" ] 1000)

let test_deal_initial_hands _ =
  let names = [ "A"; "B"; "C" ] in
  let gs = Game.create_game names 1000 in
  let players, deck' =
    Game.deal_initial_hands (Game.get_players gs) (Game.get_deck gs)
  in
  List.iter (fun p -> assert_equal 2 (List.length (Player.get_hand p))) players;
  let cards_removed = 2 * List.length names in
  assert_equal (52 - cards_removed) (List.length deck')

let test_invalid_game_creation _ =
  assert_raises (Invalid_argument "Need at least 2 players")
    (fun () -> create_game ["Solo"] 1000);
  assert_raises (Invalid_argument "Initial chips must be positive")
    (fun () -> create_game ["Player1"; "Player2"] 0)

let test_deal_initial_hands _ =
  let game = create_game ["P1"; "P2"; "P3"] 1000 in
  let players, remaining_deck = deal_initial_hands (get_players game) (get_deck game) in
  assert_equal 3 (List.length players);
  List.iter (fun p -> assert_equal 2 (List.length (get_hand p))) players;
  assert_equal 46 (List.length remaining_deck)

let test_game_getters _ =
  let game = create_game ["Alice"; "Bob"] 1000 in
  assert_equal 0 (get_pot game);
  assert_equal 2 (List.length (get_players game));
  assert_equal 52 (List.length (get_deck game));
  assert_equal [] (get_community_cards game);
  assert_equal 0 (get_current_bet game)

let test_deal_initial_hands_errors _ =
  let game = create_game ["Alice"; "Bob"] 1000 in
  (* Test dealing to empty player list *)
  assert_raises 
    (Invalid_argument "No players to deal to")
    (fun () -> deal_initial_hands [] (get_deck game));
  
  (* Test dealing with insufficient cards *)
  assert_raises
    (Failure "Not enough cards in deck to deal")
    (fun () -> deal_initial_hands (get_players game) [])

let test_deal_initial_hands_success _ =
  let game = create_game ["Alice"; "Bob"; "Charlie"] 1000 in
  let players, remaining_deck = 
    deal_initial_hands (get_players game) (get_deck game) in
  (* Each player should have 2 cards *)
  List.iter 
    (fun p -> assert_equal 2 (List.length (Player.get_hand p))) 
    players;
  (* Deck should have 52 - (2 * num_players) cards *)
  assert_equal (52 - (2 * 3)) (List.length remaining_deck)

let test_game_over_conditions _ =
  (* Test game over when all but one player has no chips *)
  let players = [
    Player.create_player 0 "Rich" 1000;
    Player.create_player 1 "Broke1" 0;
    Player.create_player 2 "Broke2" 0
  ] in
  let game = { 
    players; 
    deck = create_deck (); 
    pot = 0; 
    community_cards = []; 
    current_bet = 0 
  } in
  assert_bool "Game should be over" (is_game_over game);

  (* Test game not over when multiple players have chips *)
  let active_game = create_game ["P1"; "P2"; "P3"] 1000 in
  assert_bool "Game should not be over" (not (is_game_over active_game))

(*------------------------ Round Test ------------------------*)

(* Line 234 approx.
let test_play_round_changes_chips _ =
  Random.init 0;
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
  assert_equal (total_chips_before + 100) total_chips_after;
  let bonuses =
    List.map2
      (fun p0 p1 -> Player.get_chips p1 - Player.get_chips p0)
      (Game.get_players state0) (Game.get_players state1)
  in
  assert_equal 1 (List.length (List.filter (( <> ) 0) bonuses))
*)

(* Line 253 approx.
let test_round_betting _ =
  let initial_state = create_game ["P1"; "P2"; "P3"] 1000 in
  (* Add ante/blind bets before playing round *)
  let state_with_ante = 
    let players = get_players initial_state in
    let ante_amount = 10 in
    let players_with_ante = 
      List.map (fun p -> bet p ante_amount) players 
    in
    { initial_state with 
      players = players_with_ante;
      pot = ante_amount * List.length players 
    }
  in
  let final_state = play_round state_with_ante in
  assert_bool "Pot should be non-zero" 
    (get_pot final_state > 0);
  assert_bool "At least one player should have fewer chips"
    (List.exists (fun p -> get_chips p < 1000) (get_players final_state))
*)

(*------------------------ Hand Evaluation Tests ------------------------*)

let test_straight_flush _ =
  let hand =
    [
      make_card Ten Spades;
      make_card Jack Spades;
      make_card Queen Spades;
      make_card King Spades;
      make_card Ace Spades;
      make_card Two Diamonds;
      make_card Three Hearts;
    ]
  in
  assert_equal (StraightFlush 14) (evaluate_hand hand)

let test_four_of_a_kind _ =
  let h1 = FourOfAKind (8, make_card King Spades) in
  let h2 = FourOfAKind (8, make_card Queen Hearts) in
  assert_equal 1 (compare_hands h1 h2)

let test_full_house _ =
  let h1 = FullHouse (10, 5) in
  let h2 = FullHouse (9, 14) in
  assert_equal 1 (compare_hands h1 h2)

let test_one_pair_kicker _ =
  let h1 =
    OnePair
      (8, [ make_card King Clubs; make_card Queen Clubs; make_card Two Hearts ])
  in
  let h2 =
    OnePair
      ( 8,
        [
          make_card King Spades; make_card Jack Diamonds; make_card Three Clubs;
        ] )
  in
  assert_equal 1 (compare_hands h1 h2)

let test_high_card_tie _ =
  let h1 =
    HighCard
      [
        make_card Ace Spades;
        make_card King Hearts;
        make_card Nine Diamonds;
        make_card Five Clubs;
        make_card Two Hearts;
      ]
  in
  let h2 =
    HighCard
      [
        make_card Ace Hearts;
        make_card King Spades;
        make_card Nine Clubs;
        make_card Five Diamonds;
        make_card Two Clubs;
      ]
  in
  assert_equal 0 (compare_hands h1 h2)

let test_best_hands _ =
  let players =
    [
      ( "Alice",
        [
          make_card Ten Spades;
          make_card Jack Spades;
          make_card Queen Spades;
          make_card King Spades;
          make_card Ace Spades;
          make_card Two Hearts;
          make_card Three Diamonds;
        ] );
      ( "Bob",
        [
          make_card Nine Spades;
          make_card Nine Hearts;
          make_card Nine Clubs;
          make_card Nine Diamonds;
          make_card King Hearts;
          make_card Two Spades;
          make_card Three Spades;
        ] );
    ]
  in
  match best_hands players with
  | [ ("Alice", StraightFlush 14) ] -> ()
  | _ -> assert_failure "Expected Alice to win with a Royal Flush"

let test_evaluate_edge_cases _ =
  (* Test wheel straight *)
  let wheel = [
    {rank=Ace; suit=Hearts}; {rank=Two; suit=Hearts}; 
    {rank=Three; suit=Hearts}; {rank=Four; suit=Hearts};
    {rank=Five; suit=Hearts}; {rank=King; suit=Clubs};
    {rank=Queen; suit=Diamonds}
  ] in
  assert_equal (StraightFlush 5) (evaluate_hand wheel);

  (* Test full house with multiple possibilities *)
  let full_house = [
    {rank=Ace; suit=Hearts}; {rank=Ace; suit=Diamonds};
    {rank=Ace; suit=Clubs}; {rank=King; suit=Hearts};
    {rank=King; suit=Diamonds}; {rank=Queen; suit=Hearts};
    {rank=Jack; suit=Clubs}
  ] in
  match evaluate_hand full_house with
  | FullHouse (14, 13) -> () (* Aces full of Kings *)
  | _ -> assert_failure "Expected Aces full of Kings"

let test_string_of_hand_rank _ =
  (* Test all hand rank string representations *)
  let ace_spades = { rank = Ace; suit = Spades } in
  let king_hearts = { rank = King; suit = Hearts } in
  let queen_clubs = { rank = Queen; suit = Clubs } in
  let jack_diamonds = { rank = Jack; suit = Diamonds } in
  let ten_spades = { rank = Ten; suit = Spades } in

  assert_equal "High Card: A♠" 
    (string_of_hand_rank (HighCard [ace_spades; king_hearts; queen_clubs; jack_diamonds; ten_spades]));
  
  assert_equal "One Pair: K" 
    (string_of_hand_rank (OnePair (13, [ace_spades; queen_clubs; jack_diamonds])));
  
  assert_equal "Two Pair: K and Q" 
    (string_of_hand_rank (TwoPair (13, 12, [ace_spades])));
  
  assert_equal "Three of a Kind: J" 
    (string_of_hand_rank (ThreeOfAKind (11, [ace_spades; king_hearts])));
  
  assert_equal "Straight: A high" 
    (string_of_hand_rank (Straight 14));
  
  assert_equal "Flush: A♠ high" 
    (string_of_hand_rank (Flush [ace_spades; king_hearts; queen_clubs; jack_diamonds; ten_spades]));
  
  assert_equal "Full House: A over K" 
    (string_of_hand_rank (FullHouse (14, 13)));
  
  assert_equal "Four of a Kind: Q with A♠ kicker" 
    (string_of_hand_rank (FourOfAKind (12, ace_spades)));
  
  assert_equal "Straight Flush: A high" 
    (string_of_hand_rank (StraightFlush 14))

let test_compare_hands _ =
  (* Test comparing different hand ranks *)
  let straight_flush = StraightFlush 14 in
  let four_kind = FourOfAKind (13, {rank = Ace; suit = Spades}) in
  let full_house = FullHouse (12, 11) in
  assert_equal 1 (compare_hands straight_flush four_kind);
  assert_equal 1 (compare_hands four_kind full_house);

  (* Test comparing same hand ranks *)
  let four_kind_k = FourOfAKind (13, {rank = King; suit = Hearts}) in
  let four_kind_k_lower = FourOfAKind (13, {rank = Queen; suit = Spades}) in
  assert_equal 1 (compare_hands four_kind_k four_kind_k_lower);

  (* Test comparing flushes *)
  let flush1 = Flush [
    {rank = Ace; suit = Hearts}; 
    {rank = King; suit = Hearts};
    {rank = Queen; suit = Hearts};
    {rank = Jack; suit = Hearts};
    {rank = Ten; suit = Hearts}
  ] in
  let flush2 = Flush [
    {rank = King; suit = Spades};
    {rank = Queen; suit = Spades};
    {rank = Jack; suit = Spades};
    {rank = Ten; suit = Spades};
    {rank = Nine; suit = Spades}
  ] in
  assert_equal 1 (compare_hands flush1 flush2);

  (* Test comparing two pairs *)
  let two_pair1 = TwoPair (14, 13, [{rank = Queen; suit = Hearts}]) in
  let two_pair2 = TwoPair (14, 12, [{rank = Jack; suit = Spades}]) in
  assert_equal 1 (compare_hands two_pair1 two_pair2)

let test_invalid_comparisons _ =
  let invalid_flush1 = Flush [] in
  let invalid_flush2 = Flush [{rank = Ace; suit = Spades}] in
  assert_raises (Failure "Invalid flush comparison")
    (fun () -> compare_hands invalid_flush1 invalid_flush2)

(*------------------------ Aggregated Test Suite ------------------------*)

let suite =
  "Full OCaml Poker Test Suite"
  >::: [
         (* Card module *)
         "deck size" >:: test_deck_size;
         "deck uniqueness" >:: test_deck_uniqueness;
         "card creation" >:: test_card_creation;
         "all ranks" >:: test_all_ranks;
         "all suits" >:: test_all_suits;
         "string of card" >:: test_string_of_card;
         "create deck" >:: test_create_deck;
         "card basics" >:: test_card_basics;
         "deck creation" >:: test_deck_creation;
         "int to rank conversion" >:: test_int_to_rank;
         "string of rank" >:: test_string_of_rank;
         (* Player module *)
         "negative chips" >:: test_negative_chips;
         "player accessors" >:: test_accessor_roundtrip;
         "player actions" >:: test_player_actions;
         (* Game module *)
         "create_game invalid chips" >:: test_create_game_invalid_chip;
         "create_game single player" >:: test_create_game_insufficient_players;
         "deal_initial_hands" >:: test_deal_initial_hands;
         "invalid game creation" >:: test_invalid_game_creation;
         "deal_initial_hands" >:: test_deal_initial_hands;
         "game getters" >:: test_game_getters;
         "deal initial hands errors" >:: test_deal_initial_hands_errors;
         "deal initial hands success" >:: test_deal_initial_hands_success;
         "game over conditions" >:: test_game_over_conditions;
         (* Round logic *)
         (* "play_round chip changes" >:: test_play_round_changes_chips; *)
         (* "round betting" >:: test_round_betting; *)
         (* Hand ranking and comparison *)
         "Straight Flush" >:: test_straight_flush;
         "Four of a Kind" >:: test_four_of_a_kind;
         "Full House" >:: test_full_house;
         "One Pair Kicker" >:: test_one_pair_kicker;
         "High Card Tie" >:: test_high_card_tie;
         "Best Hands" >:: test_best_hands;
         "Evaluate Edge Cases" >:: test_evaluate_edge_cases;
         "string of hand rank" >:: test_string_of_hand_rank;
         "compare hands" >:: test_compare_hands;
         "invalid comparisons" >:: test_invalid_comparisons;
       ]

let () = run_test_tt_main suite
