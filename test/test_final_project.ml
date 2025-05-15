(* test/test_final_project.ml *)
open OUnit2
open Final_project
open Card
open Player
open Game
open Final_project.Round
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

let test_string_of_card_all _ =
  List.iter
    (fun rank ->
      List.iter
        (fun suit ->
          let c = { rank; suit } in
          let str = string_of_card c in
          assert_bool "non-empty string" (String.length str > 0))
        all_suits)
    all_ranks

let test_int_to_rank_roundtrip _ =
  List.iter2
    (fun i r -> assert_equal r (int_to_rank i))
    (List.init 13 (fun k -> k + 2))
    all_ranks

let test_int_to_rank_failure _ =
  assert_raises (Failure "Invalid rank value") (fun () -> int_to_rank 1)

let test_string_of_rank_all _ =
  List.iter
    (fun r -> assert_bool "non-empty" (String.length (string_of_rank r) > 0))
    all_ranks

let test_create_deck_contents _ =
  let deck = create_deck () in
  assert_equal 52 (List.length deck);
  assert_equal 52 (List.length (List.sort_uniq compare deck));
  List.iter
    (fun r ->
      List.iter
        (fun s -> assert (List.mem { rank = r; suit = s } deck))
        all_suits)
    all_ranks

(*------------------------ Player Tests ------------------------*)
let test_bet_paths _ =
  let p = Player.create_player 0 "P" 100 in
  (* negative bet -> error *)
  assert_raises (Invalid_argument "Bet amount cannot be negative") (fun () ->
      Player.bet p (-1));
  (* normal bet 40 *)
  let p40 = Player.bet p 40 in
  assert_equal 60 (Player.get_chips p40);
  assert_equal 40 (Player.get_current_bet p40);
  assert_bool "not all‑in" (not (Player.is_all_in p40));
  (* all‑in bet equal to remaining chips *)
  let p_all = Player.bet p40 60 in
  assert_equal 0 (Player.get_chips p_all);
  assert_equal true (Player.is_all_in p_all)

let test_call_paths _ =
  let base = Player.set_current_bet (Player.create_player 1 "C" 50) 10 in
  (* game bet already matched – nothing happens *)
  let same = Player.call base 10 in
  assert_equal 50 (Player.get_chips same);
  (* call that forces all‑in *)
  let p_all = Player.call base 100 in
  assert_equal 0 (Player.get_chips p_all);
  assert_equal true (Player.is_all_in p_all)

let test_check_paths _ =
  let p = Player.set_current_bet (Player.create_player 2 "Chk" 30) 15 in
  ignore (Player.check p 15);
  (* valid *)
  assert_raises (Invalid_argument "Cannot check when there is a bet to call")
    (fun () -> Player.check p 20)

let test_raise_paths _ =
  let p0 = Player.set_current_bet (Player.create_player 3 "R" 100) 20 in
  (* raise amount non‑positive *)
  assert_raises
    (Invalid_argument
       "Raise amount must be greater than current bet contribution") (fun () ->
      Player.raise_bet p0 20);
  (* raise > stack -> all‑in *)
  let p_all = Player.raise_bet p0 200 in
  assert_equal 0 (Player.get_chips p_all);
  assert_equal true (Player.is_all_in p_all);
  (* raise exactly to all‑in boundary *)
  let p_exact =
    Player.set_current_bet (Player.create_player 4 "R2" 80) 20 |> fun p ->
    Player.raise_bet p 100
  in
  assert_equal 0 (Player.get_chips p_exact);
  assert_equal true (Player.is_all_in p_exact);
  (* normal raise *)
  let p_norm =
    Player.create_player 5 "R3" 150 |> fun p ->
    Player.set_current_bet p 30 |> fun p -> Player.raise_bet p 90
  in
  assert_equal 90 (Player.get_current_bet p_norm);
  assert_equal 90 (Player.get_chips p_norm)

let test_setters_and_reset _ =
  let p = Player.create_player 6 "S" 10 in
  let p' = Player.set_folded (Player.set_all_in p true) true in
  assert_bool "all‑in flag" (Player.is_all_in p');
  assert_bool "folded flag" (Player.is_folded p');
  let p'' = Player.update_chips p' 20 in
  assert_equal 30 (Player.get_chips p'');
  let fresh = Player.reset_for_new_hand p'' in
  assert_bool "reset folded" (not (Player.is_folded fresh));
  assert_equal 0 (Player.get_current_bet fresh);
  assert_equal false (Player.is_all_in fresh)

(* ------------------------------------------------------------------ *)
(* Player deep-dive – exercise every public branch                    *)
(* ------------------------------------------------------------------ *)

let test_bet_negative _ =
  let p = Player.create_player 0 "P" 100 in
  assert_raises (Invalid_argument "Bet amount cannot be negative") (fun () ->
      Player.bet p (-5))

let test_bet_insufficient _ =
  let p = Player.create_player 0 "P" 50 in
  assert_raises (Invalid_argument "Insufficient chips") (fun () ->
      Player.bet p 60)

let test_bet_partial_and_all_in _ =
  let p1 = Player.create_player 0 "P" 120 in
  let p1' = Player.bet p1 20 in
  assert_equal 100 (Player.get_chips p1');
  assert_equal 20 (Player.get_current_bet p1');
  assert_bool "not all-in" (not (Player.is_all_in p1'));

  let p2 = Player.create_player 0 "P" 80 in
  let p2' = Player.bet p2 80 in
  assert_equal 0 (Player.get_chips p2');
  assert_bool "all-in" (Player.is_all_in p2')

let test_call_paths _ =
  (* already matched – no change *)
  let p0 =
    Player.create_player 0 "P" 100 |> fun p -> Player.set_current_bet p 40
  in
  let p0' = Player.call p0 40 in
  assert_equal (Player.get_chips p0) (Player.get_chips p0');

  (* partial call *)
  let p1 =
    Player.create_player 0 "P" 60 |> fun p -> Player.set_current_bet p 10
  in
  let p1' = Player.call p1 40 in
  (* needs 30 *)
  assert_equal 30 (Player.get_chips p1');
  assert_equal 40 (Player.get_current_bet p1');

  (* all-in call *)
  let p2 =
    Player.create_player 0 "P" 25 |> fun p -> Player.set_current_bet p 5
  in
  let p2' = Player.call p2 100 in
  (* cannot cover *)
  assert_equal 0 (Player.get_chips p2');
  assert_bool "all-in flag" (Player.is_all_in p2')

let test_check_success_and_fail _ =
  let p_ok = Player.set_current_bet (Player.create_player 0 "P" 100) 30 in
  ignore (Player.check p_ok 30);

  (* should not raise *)
  let p_bad = Player.set_current_bet p_ok 10 in
  assert_raises (Invalid_argument "Cannot check when there is a bet to call")
    (fun () -> Player.check p_bad 30)

let test_raise_paths _ =
  (* invalid raise amount (<= current) *)
  let p = Player.set_current_bet (Player.create_player 0 "P" 100) 50 in
  assert_raises
    (Invalid_argument
       "Raise amount must be greater than current bet contribution") (fun () ->
      Player.raise_bet p 50);

  (* raise puts player all-in because stack < diff *)
  let p2 =
    Player.create_player 0 "P" 30 |> fun p -> Player.set_current_bet p 20
  in
  let p2' = Player.raise_bet p2 60 in
  assert_equal 0 (Player.get_chips p2');
  assert_bool "all-in flag" (Player.is_all_in p2');

  (* normal raise within stack *)
  let p3 = Player.set_current_bet (Player.create_player 0 "P" 200) 20 in
  let p3' = Player.raise_bet p3 80 in
  assert_equal 140 (Player.get_chips p3');
  assert_equal 80 (Player.get_current_bet p3');
  assert_bool "not all-in flag" (not (Player.is_all_in p3'))

let test_reset_and_update _ =
  let p0 =
    Player.set_all_in
      (Player.set_current_bet
         (Player.set_folded (Player.create_player 0 "P" 100) true)
         40)
      true
  in
  let p1 = Player.reset_for_new_hand p0 in
  assert_bool "fold cleared" (not (Player.is_folded p1));
  assert_equal 0 (Player.get_current_bet p1);
  assert_bool "all_in cleared" (not (Player.is_all_in p1));

  let p2 = Player.update_chips p1 50 in
  assert_equal 150 (Player.get_chips p2)

(*------------------------ Game Tests ------------------------*)

let test_create_game_invalid _ =
  assert_raises (Invalid_argument "Initial chips must be positive") (fun () ->
      Game.create_game [ "A"; "B" ] 0);
  assert_raises (Invalid_argument "Need at least 2 players") (fun () ->
      Game.create_game [ "Solo" ] 1000)

let test_deal_initial_hands_errors _ =
  let g = Game.create_game [ "A"; "B" ] 1000 in
  assert_raises (Invalid_argument "No players to deal to") (fun () ->
      Game.deal_initial_hands [] (Game.get_deck g));
  assert_raises (Failure "Not enough cards to deal initial hands") (fun () ->
      Game.deal_initial_hands (Game.get_players g) [])

let test_deal_initial_hands_success _ =
  let g = Game.create_game [ "A"; "B"; "C" ] 1000 in
  let players, deck =
    Game.deal_initial_hands (Game.get_players g) (Game.get_deck g)
  in
  List.iter (fun p -> assert_equal 2 (List.length (Player.get_hand p))) players;
  assert_equal 46 (List.length deck)

let test_game_over_conditions _ =
  let ps =
    [
      Player.create_player 0 "A" 0;
      Player.create_player 1 "B" 0;
      Player.create_player 2 "C" 1000;
    ]
  in
  let g =
    {
      players = ps;
      deck = create_deck ();
      pot = 0;
      community_cards = [];
      current_bet = 0;
    }
  in
  assert_bool "Should be over" (is_game_over g)

let test_game_not_over _ =
  let g = Game.create_game [ "A"; "B"; "C" ] 1000 in
  assert_bool "Should not be over" (not (is_game_over g))

let test_reveal_community_cards _ =
  let g = Game.create_game [ "A"; "B" ] 1000 in
  let players, deck =
    Game.deal_initial_hands (Game.get_players g) (Game.get_deck g)
  in
  let g = { g with players; deck } in
  let g2 = Game.reveal_community_cards g 3 in
  assert_equal 3 (List.length (Game.get_community_cards g2));
  assert_equal 0 (Game.get_current_bet g2);
  List.iter
    (fun p -> assert_equal 0 (Player.get_current_bet p))
    (Game.get_players g2)

let test_distribute_pot _ =
  let ps = [ Player.create_player 0 "A" 100; Player.create_player 1 "B" 100 ] in
  let g =
    {
      players = ps;
      deck = [];
      pot = 200;
      community_cards = [];
      current_bet = 0;
    }
  in
  let g' = Game.distribute_pot g [ 0; 1 ] in
  List.iter
    (fun p -> assert_equal 200 (Player.get_chips p))
    (Game.get_players g')

let test_handle_bet_and_raise _ =
  let g = Game.create_game [ "A"; "B" ] 1000 in
  let g1 = Game.handle_player_bet g 0 200 in
  let g2 = Game.handle_player_raise g1 1 500 in
  assert_equal 700 (Player.get_chips (List.nth (Game.get_players g2) 1));
  assert_equal 700 (Game.get_pot g2);
  assert_equal 500 (Game.get_current_bet g2)

let test_handle_call _ =
  let g = Game.create_game [ "A"; "B" ] 500 in
  let g1 = Game.handle_player_bet g 0 100 in
  let g2 = Game.handle_player_call g1 1 in
  assert_equal 100 (Player.get_current_bet (List.nth (Game.get_players g2) 1));
  assert_equal 200 (Game.get_pot g2)

let test_handle_check_invalid _ =
  let g = Game.create_game [ "A"; "B" ] 1000 in
  let g = { g with current_bet = 100 } in
  assert_raises (Invalid_argument "Cannot check when there is a bet to call")
    (fun () -> Game.handle_player_check g 0)

let test_new_hand_resets_state _ =
  let g = Game.create_game [ "A"; "B" ] 500 in
  let g' = Game.new_hand g in
  List.iter
    (fun p -> assert_equal 2 (List.length (Player.get_hand p)))
    (Game.get_players g');
  assert_equal 0 (Game.get_current_bet g');
  assert_equal 0 (Game.get_pot g')

(*------------------------ Hand Evaluation Tests ------------------------*)

(* helper to assert evaluation kind quickly *)
let assert_eval ~msg expected seven =
  let got = evaluate_hand seven in
  if compare_hands expected got <> 0 then
    assert_failure (msg ^ ": unexpected rank")

(* ------------- individual rank detection -------------------------- *)

let test_high_card_eval _ =
  let seven =
    [
      make_card Ace Spades;
      make_card Jack Hearts;
      make_card Nine Clubs;
      make_card Seven Diamonds;
      make_card Five Spades;
      make_card Four Hearts;
      make_card Two Clubs;
    ]
  in
  assert_eval ~msg:"HighCard" (HighCard []) seven (* kicker list ignored *)

let test_one_pair_eval _ =
  let seven =
    [
      make_card Jack Spades;
      make_card Jack Hearts;
      make_card Nine Clubs;
      make_card Five Diamonds;
      make_card Three Hearts;
      make_card Two Spades;
      make_card Four Clubs;
    ]
  in
  match evaluate_hand seven with
  | OnePair (11, _) -> ()
  | _ -> assert_failure "expect OnePair JJ"

let test_two_pair_eval _ =
  let seven =
    [
      make_card King Spades;
      make_card King Hearts;
      make_card Nine Diamonds;
      make_card Nine Spades;
      make_card Two Clubs;
      make_card Four Hearts;
      make_card Six Clubs;
    ]
  in
  match evaluate_hand seven with
  | TwoPair (13, 9, _) -> ()
  | _ -> assert_failure "expect KK & 99"

let test_three_kind_eval _ =
  let seven =
    [
      make_card Four Spades;
      make_card Four Diamonds;
      make_card Four Clubs;
      make_card Ace Hearts;
      make_card Nine Spades;
      make_card Two Clubs;
      make_card Six Hearts;
    ]
  in
  match evaluate_hand seven with
  | ThreeOfAKind (4, _) -> ()
  | _ -> assert_failure "expect trip 4s"

let test_straight_eval _ =
  let seven =
    [
      make_card Four Clubs;
      make_card Five Diamonds;
      make_card Six Hearts;
      make_card Seven Spades;
      make_card Eight Hearts;
      make_card King Clubs;
      make_card Ace Diamonds;
    ]
  in
  match evaluate_hand seven with
  | Straight 8 -> ()
  | _ -> assert_failure "expect 8-high straight"

let test_straight_ace_low_eval _ =
  let seven =
    [
      make_card Ace Spades;
      make_card Two Hearts;
      make_card Three Diamonds;
      make_card Four Clubs;
      make_card Five Spades;
      make_card Nine Hearts;
      make_card Ten Clubs;
    ]
  in
  match evaluate_hand seven with
  | Straight 5 -> ()
  | _ -> assert_failure "expect 5-high straight (A-low)"

let test_flush_eval _ =
  let seven =
    [
      make_card Ace Hearts;
      make_card Ten Hearts;
      make_card Seven Hearts;
      make_card Six Hearts;
      make_card Two Hearts;
      make_card Nine Clubs;
      make_card Four Diamonds;
    ]
  in
  match evaluate_hand seven with
  | Flush cs ->
      assert_equal 5 (List.length cs);
      assert_equal Hearts (List.hd cs).suit
  | _ -> assert_failure "expect hearts flush"

let test_full_house_eval _ =
  let seven =
    [
      make_card Six Hearts;
      make_card Six Spades;
      make_card Six Diamonds;
      make_card Two Clubs;
      make_card Two Diamonds;
      make_card King Hearts;
      make_card Queen Clubs;
    ]
  in
  match evaluate_hand seven with
  | FullHouse (6, 2) -> ()
  | _ -> assert_failure "expect 6 over 2"

let test_four_kind_eval _ =
  let seven =
    [
      make_card Eight Spades;
      make_card Eight Hearts;
      make_card Eight Clubs;
      make_card Eight Diamonds;
      make_card Ace Hearts;
      make_card Two Clubs;
      make_card Three Spades;
    ]
  in
  match evaluate_hand seven with
  | FourOfAKind (8, _) -> ()
  | _ -> assert_failure "expect quad 8s"

let test_straight_flush_eval _ =
  let seven =
    [
      make_card Nine Hearts;
      make_card Ten Hearts;
      make_card Jack Hearts;
      make_card Queen Hearts;
      make_card King Hearts;
      make_card Two Spades;
      make_card Three Clubs;
    ]
  in
  match evaluate_hand seven with
  | StraightFlush 13 -> ()
  | _ -> assert_failure "expect 13-high SF"

(* ------------- compare_hands deep branches ------------------------ *)

let test_compare_flush_kickers _ =
  let f1 =
    Flush
      [
        make_card Ace Spades;
        make_card Ten Spades;
        make_card Seven Spades;
        make_card Five Spades;
        make_card Three Spades;
      ]
  and f2 =
    Flush
      [
        make_card King Spades;
        make_card Ten Spades;
        make_card Seven Spades;
        make_card Five Spades;
        make_card Three Spades;
      ]
  in
  assert_equal 1 (compare_hands f1 f2)

let test_compare_full_house _ =
  let h1 = FullHouse (7, 2) and h2 = FullHouse (6, 14) in
  assert_equal 1 (compare_hands h1 h2)

let test_compare_four_kind_kicker _ =
  let k = make_card Ace Spades and q = make_card Queen Hearts in
  let h1 = FourOfAKind (9, k) and h2 = FourOfAKind (9, q) in
  assert_equal 1 (compare_hands h1 h2)

let test_compare_rank_strength_diff _ =
  let straight = Straight 10 and trips = ThreeOfAKind (5, []) in
  assert_equal 1 (compare_hands straight trips)
(* ------------------------------------------------------------------ *)
(* Round – helper-function coverage                                   *)
(* ------------------------------------------------------------------ *)

let test_reset_folds _ =
  let ps =
    [
      Player.set_folded (Player.create_player 0 "A" 100) true;
      Player.create_player 1 "B" 100;
    ]
  in
  let ps' = Round.reset_folds ps in
  List.iter (fun p -> assert_bool "fold cleared" (not (Player.is_folded p))) ps'

let test_reset_player_bets _ =
  let g = Game.create_game [ "A"; "B" ] 500 in
  let p0 = Player.set_current_bet (List.nth (Game.get_players g) 0) 50 in
  let g =
    {
      g with
      players = [ p0; List.nth (Game.get_players g) 1 ];
      current_bet = 50;
    }
  in
  let g' = Round.reset_player_bets_for_street g in
  assert_equal 0 (Game.get_current_bet g');
  List.iter
    (fun p -> assert_equal 0 (Player.get_current_bet p))
    (Game.get_players g')

let test_get_starting_player_index _ =
  let ps =
    [
      Player.set_folded (Player.create_player 0 "A" 100) true;
      Player.create_player 1 "B" 0;
      (* no chips *)
      Player.create_player 2 "C" 100;
    ]
  in
  assert_equal 2 (Round.get_starting_player_index ps 0)

let test_handle_bot_action_check_branch _ =
  (* bot has 0 chips so branch is deterministic: `Check` *)
  let g = Game.create_game [ "Bot" ] 0 in
  let bot = List.hd (Game.get_players g) in
  let g', _, was_aggr = Round.handle_bot_action g bot 10 in
  assert_equal 0 (Game.get_pot g');
  (* check does not change pot *)
  assert_bool "not aggressive" (not was_aggr)
(* ------------- string_of_hand_rank every branch ------------------ *)

let test_string_of_hand_rank_all _ =
  let samples =
    [
      HighCard [ make_card Ace Spades ];
      OnePair (11, []);
      TwoPair (13, 9, []);
      ThreeOfAKind (4, []);
      Straight 8;
      Flush [ make_card King Clubs ];
      FullHouse (6, 2);
      FourOfAKind (7, make_card Jack Hearts);
      StraightFlush 14;
    ]
  in
  List.iter
    (fun hr ->
      let s = string_of_hand_rank hr in
      assert_bool "non-empty string_of_hand_rank" (String.length s > 0))
    samples

(* ------------- evaluate_hand length error already covered earlier *)

(*------------------------ Aggregated Test Suite ------------------------*)

let suite =
  "Full OCaml Poker Test Suite"
  >::: [
         (* Card *)
         "string_of_card all" >:: test_string_of_card_all;
         "int_to_rank round-trip" >:: test_int_to_rank_roundtrip;
         "int_to_rank failure" >:: test_int_to_rank_failure;
         "string_of_rank all" >:: test_string_of_rank_all;
         "create_deck contents" >:: test_create_deck_contents;
         (* Player *)
         "bet paths" >:: test_bet_paths;
         "call paths" >:: test_call_paths;
         "check paths" >:: test_check_paths;
         "raise paths" >:: test_raise_paths;
         "setters & reset" >:: test_setters_and_reset;
         (* Game *)
         "create_game invalid" >:: test_create_game_invalid;
         "deal_initial_hands errors" >:: test_deal_initial_hands_errors;
         "deal_initial_hands success" >:: test_deal_initial_hands_success;
         "game over" >:: test_game_over_conditions;
         "game not over" >:: test_game_not_over;
         "reveal community cards" >:: test_reveal_community_cards;
         "distribute pot" >:: test_distribute_pot;
         "handle bet and raise" >:: test_handle_bet_and_raise;
         "handle call" >:: test_handle_call;
         "handle check invalid" >:: test_handle_check_invalid;
         "new hand resets state" >:: test_new_hand_resets_state;
         (* Hand *)
         (* Hand deep-dive *)
         "high card eval" >:: test_high_card_eval;
         "one pair eval" >:: test_one_pair_eval;
         "two pair eval" >:: test_two_pair_eval;
         "three kind eval" >:: test_three_kind_eval;
         "straight eval" >:: test_straight_eval;
         "straight ace-low" >:: test_straight_ace_low_eval;
         "flush eval" >:: test_flush_eval;
         "full house eval" >:: test_full_house_eval;
         "four kind eval" >:: test_four_kind_eval;
         "straight flush eval" >:: test_straight_flush_eval;
         "compare flush kickers" >:: test_compare_flush_kickers;
         "compare full house" >:: test_compare_full_house;
         "compare four kind kicker" >:: test_compare_four_kind_kicker;
         "compare rank-strength diff" >:: test_compare_rank_strength_diff;
         "string_of_hand_rank all" >:: test_string_of_hand_rank_all;
         (* Player deep-dive *)
         "bet negative" >:: test_bet_negative;
         "bet insufficient" >:: test_bet_insufficient;
         "bet partial & all-in" >:: test_bet_partial_and_all_in;
         "call paths" >:: test_call_paths;
         "check success/fail" >:: test_check_success_and_fail;
         "raise paths" >:: test_raise_paths;
         "reset & update" >:: test_reset_and_update;

         "reset folds" >:: test_reset_folds;
         "reset player bets" >:: test_reset_player_bets;
         "starting player index" >:: test_get_starting_player_index;
         "bot action (check)" >:: test_handle_bot_action_check_branch;
       ]

let () = run_test_tt_main suite
