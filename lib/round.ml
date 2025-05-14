open Game
open Player
  open Ui
open Card 

let () = Random.self_init ()  
  
  let reset_folds (players : Player.t list) =
    List.map (fun p -> Player.set_folded p false) players
  
let reset_player_bets_for_street (gs: Game.t) : Game.t =
  let updated_players = List.map (fun p -> Player.set_current_bet p 0) (Game.get_players gs) in
  { gs with players = updated_players; current_bet = 0 }
 
let get_starting_player_index (players : Player.t list) (dealer_button_pos: int) : int =
  let num_players = List.length players in
  if num_players = 0 then 0  
  else
    let rec find_next_active offset count =
      if count >= num_players then  
        (match List.find_index (fun p -> not (Player.is_folded p) && Player.get_chips p > 0) players with Some i -> i | None -> 0)
      else 
        let p_idx = (dealer_button_pos + 1 + offset) mod num_players in  
        let player = List.nth players p_idx in
        if not (Player.is_folded player) && Player.get_chips player > 0 then p_idx
        else find_next_active (offset + 1) (count + 1)
    in
    find_next_active 0 0

let handle_bot_action gs bot_player min_bet_val =
  Printf.printf "Turn for %s (Bot). Waiting for Bot action...\n" (Player.get_name bot_player);
  Unix.sleepf 0.5;

  let game_current_bet = Game.get_current_bet gs in
  let bot_chips = Player.get_chips bot_player in
  let bot_committed_bet = Player.get_current_bet bot_player in
  let can_check = game_current_bet = 0 || bot_committed_bet = game_current_bet in
  let amount_to_call = game_current_bet - bot_committed_bet in

  let action =
    let rand_val = Random.float 1.0 in
    if can_check then
      if bot_chips = 0 then `Check
      else if rand_val < 0.50 then `Check
      else if rand_val < 0.80 then `Bet
      else if rand_val < 0.95 && bot_chips > 0 then `AllIn
      else `Fold
    else 
      if bot_chips <= amount_to_call && bot_chips > 0 then `AllIn
      else if bot_chips = 0 then `Fold
      else if rand_val < 0.60 && bot_chips >= amount_to_call then `Call
      else if rand_val < 0.85 && bot_chips > amount_to_call then `Raise
      else if rand_val < 0.95 && bot_chips > 0 then `AllIn
      else `Fold
  in

  match action with
  | `Check ->
      let next_gs = Game.handle_player_check gs (Player.get_id bot_player) in
      Printf.printf "%s (Bot) checks.\n" (Player.get_name bot_player);
      (next_gs, Some (Player.get_id bot_player), false)
  | `Bet ->
      let bet_amount = max min_bet_val (min bot_chips (min_bet_val + Random.int (bot_chips / 4 + 1))) in
      if bot_chips = 0 then ( 
        let next_gs = Game.handle_player_check gs (Player.get_id bot_player) in
        Printf.printf "%s (Bot) checks (no chips to bet).\n" (Player.get_name bot_player);
        (next_gs, Some (Player.get_id bot_player), false)
      ) else if bet_amount > 0 then (
        let next_gs = Game.handle_player_bet gs (Player.get_id bot_player) bet_amount in
        Printf.printf "%s (Bot) bets %d.\n" (Player.get_name bot_player) bet_amount;
        (next_gs, Some (Player.get_id bot_player), true)
      ) else ( 
        let next_gs = Game.handle_player_check gs (Player.get_id bot_player) in
        Printf.printf "%s (Bot) checks (bet calculation error).\n" (Player.get_name bot_player);
        (next_gs, Some (Player.get_id bot_player), false)
      )
  | `Call ->
      let next_gs = Game.handle_player_call gs (Player.get_id bot_player) in
      Printf.printf "%s (Bot) calls %d.\n" (Player.get_name bot_player) amount_to_call;
      (next_gs, None, false)
  | `Raise ->
      let min_raise_total = game_current_bet + max min_bet_val (game_current_bet - bot_committed_bet) in
      let raise_amount = max min_raise_total (min (bot_chips + bot_committed_bet) (min_raise_total + Random.int (bot_chips / 3 + 1))) in
      if bot_chips > amount_to_call && raise_amount > game_current_bet && raise_amount <= bot_chips + bot_committed_bet then (
        let next_gs = Game.handle_player_raise gs (Player.get_id bot_player) raise_amount in
        Printf.printf "%s (Bot) raises to %d.\n" (Player.get_name bot_player) raise_amount;
        (next_gs, Some (Player.get_id bot_player), true)
      ) else ( 
        let next_gs = Game.handle_player_call gs (Player.get_id bot_player) in
        Printf.printf "%s (Bot) calls %d (raise calculation error).\n" (Player.get_name bot_player) amount_to_call;
        (next_gs, None, false)
      )
  | `AllIn ->
      let all_in_amount_as_bet = bot_chips in
      let total_final_bet = bot_chips + bot_committed_bet in
      let next_gs, new_aggressor, was_aggressive = 
        if game_current_bet = 0 || total_final_bet > game_current_bet then
          if game_current_bet = 0 then 
            (Game.handle_player_bet gs (Player.get_id bot_player) all_in_amount_as_bet, Some (Player.get_id bot_player), true)
          else 
            (Game.handle_player_raise gs (Player.get_id bot_player) total_final_bet, Some (Player.get_id bot_player), true)
        else 
           (Game.handle_player_call gs (Player.get_id bot_player), None, false) 
      in
      Printf.printf "%s (Bot) goes all-in! (Total: %d)\n" (Player.get_name bot_player) total_final_bet;
      (next_gs, new_aggressor, was_aggressive)
  | `Fold ->
      let next_gs = Game.handle_player_fold gs (Player.get_id bot_player) in
      Printf.printf "%s (Bot) folds.\n" (Player.get_name bot_player);
      (next_gs, None, false)

let rec betting_round (gs : Game.t) (player_idx_to_act : int) (min_bet_val : int) : Game.t =
  Printf.printf "\n--- Betting Round ---\n";
  let initial_players = Game.get_players gs in
  let num_total_players = List.length initial_players in

  let rec loop current_gs current_player_idx aggressor_id players_who_have_acted num_actions_this_bet_cycle =
    let current_players = Game.get_players current_gs in
    let player_to_act = List.nth current_players current_player_idx in
 
    let players_still_in_hand = List.filter (fun p -> not (Player.is_folded p)) current_players in 
    if List.length players_still_in_hand <= 1 then current_gs  
    else if num_actions_this_bet_cycle >= List.length players_still_in_hand && 
            ( (match aggressor_id with None -> true | Some _ -> false) || 
              (match aggressor_id with Some id -> Player.get_id player_to_act = id | None -> false)
            )
            && List.for_all (fun p -> 
                Player.is_folded p || 
                Player.get_chips p = 0 ||  
                Player.is_all_in p ||  
                Player.get_current_bet p = Game.get_current_bet current_gs
            ) players_still_in_hand  
    then current_gs
    else if Player.is_folded player_to_act || (Player.is_all_in player_to_act ) then 
      loop current_gs ((current_player_idx + 1) mod num_total_players) aggressor_id players_who_have_acted (num_actions_this_bet_cycle +1)
    else
      let _ = Ui.print_game_state current_gs current_player_idx in
      let new_gs, new_aggressor_id, was_aggressive_action =
        if Player.get_name player_to_act = "You" then begin
          Ui.print_action_menu current_gs player_to_act min_bet_val;
          match Ui.get_player_action current_gs player_to_act min_bet_val with
          | Check -> 
              let next_gs = Game.handle_player_check current_gs (Player.get_id player_to_act) in
              Printf.printf "%s checks.\n" (Player.get_name player_to_act);
              (next_gs, aggressor_id, false)
          | Bet amount -> 
              let next_gs = Game.handle_player_bet current_gs (Player.get_id player_to_act) amount in
              Printf.printf "%s bets %d.\n" (Player.get_name player_to_act) amount;
              (next_gs, Some (Player.get_id player_to_act), true)
          | Call -> 
              let next_gs = Game.handle_player_call current_gs (Player.get_id player_to_act) in
              Printf.printf "%s calls.\n" (Player.get_name player_to_act);
              (next_gs, aggressor_id, false)
          | Raise amount -> 
              let next_gs = Game.handle_player_raise current_gs (Player.get_id player_to_act) amount in
              Printf.printf "%s raises to %d.\n" (Player.get_name player_to_act) amount;
              (next_gs, Some (Player.get_id player_to_act), true)
          | Fold -> 
              let next_gs = Game.handle_player_fold current_gs (Player.get_id player_to_act) in
              Printf.printf "%s folds.\n" (Player.get_name player_to_act);
              (next_gs, aggressor_id, false)
          | AllIn -> 
              let chips = Player.get_chips player_to_act in
              let player_curr_bet_val = Player.get_current_bet player_to_act in
              let game_curr_bet_val = Game.get_current_bet current_gs in
              let total_bet_value = chips + player_curr_bet_val in
              let next_gs, resulting_aggressor_id, resulting_was_aggressive = 
                if game_curr_bet_val = 0 || total_bet_value > game_curr_bet_val then
                  if game_curr_bet_val = 0 then (Game.handle_player_bet current_gs (Player.get_id player_to_act) chips), Some (Player.get_id player_to_act), true
                  else (Game.handle_player_raise current_gs (Player.get_id player_to_act) total_bet_value), Some (Player.get_id player_to_act), true
                else 
                  (Game.handle_player_call current_gs (Player.get_id player_to_act)), aggressor_id, false
              in
              Printf.printf "%s goes all-in! (Total: %d)\n" (Player.get_name player_to_act) total_bet_value;
              (next_gs, resulting_aggressor_id, resulting_was_aggressive)
        end else begin
          handle_bot_action current_gs player_to_act min_bet_val
        end
      in
      let next_player_idx = (current_player_idx + 1) mod num_total_players in
      let updated_players_acted = 
        if List.mem (Player.get_id player_to_act) players_who_have_acted then players_who_have_acted 
        else Player.get_id player_to_act :: players_who_have_acted 
      in
      let new_num_actions = if was_aggressive_action then 1 else num_actions_this_bet_cycle + 1 in
      let final_aggressor_id = if was_aggressive_action then new_aggressor_id else aggressor_id in 
      loop new_gs next_player_idx final_aggressor_id updated_players_acted new_num_actions
  in
  let gs_street_ready = { gs with current_bet = 0; players = List.map (fun p -> Player.set_current_bet p 0) (Game.get_players gs) } in
  loop gs_street_ready player_idx_to_act None [] 0

let play_hand_stages (state : Game.t) (dealer_button_pos: int) (big_blind_val: int): Game.t =
    print_endline "\n=====================";
    print_endline "   Starting New Hand ";
    print_endline "=====================";
  
    let hand_state = Game.new_hand state in 
  
  (* TODO: Implement posting blinds. For now, min_bet_val is big_blind_val *)
  let min_bet_for_round = big_blind_val in 

    print_endline "-- Pre-Flop --";
  Ui.print_game_state hand_state dealer_button_pos;
  let starting_player_preflop = get_starting_player_index (Game.get_players hand_state) dealer_button_pos in (* Adjust for blinds *)
  let state_after_preflop_betting = betting_round hand_state starting_player_preflop min_bet_for_round in
  
  let active_players_preflop = List.filter (fun p -> not (Player.is_folded p)) (Game.get_players state_after_preflop_betting) in
  if List.length active_players_preflop <= 1 then (
    match active_players_preflop with
    | [winner] -> 
        Printf.printf "\n🏆 Winner as only remaining player: %s\n" (Player.get_name winner);
        let final_state = Game.distribute_pot state_after_preflop_betting [Player.get_id winner] in
        Printf.printf "\n--- End of Hand ---";
        Ui.print_game_state final_state dealer_button_pos;
        final_state
    | _ -> state_after_preflop_betting  
  )
  else
    let state_for_flop = reset_player_bets_for_street state_after_preflop_betting in
    let state_after_flop_deal = Game.reveal_community_cards state_for_flop 3 in
    print_endline "\n-- Flop --";
    Ui.print_game_state state_after_flop_deal dealer_button_pos;
    let starting_player_flop = get_starting_player_index (Game.get_players state_after_flop_deal) dealer_button_pos in 
    let state_after_flop_betting = betting_round state_after_flop_deal starting_player_flop min_bet_for_round in

    let active_players_flop = List.filter (fun p -> not (Player.is_folded p)) (Game.get_players state_after_flop_betting) in
    if List.length active_players_flop <= 1 then (
      match active_players_flop with
      | [winner] -> 
          Printf.printf "\n🏆 Winner as only remaining player: %s\n" (Player.get_name winner);
          let final_state = Game.distribute_pot state_after_flop_betting [Player.get_id winner] in
          Printf.printf "\n--- End of Hand ---";
          Ui.print_game_state final_state dealer_button_pos;
          final_state
      | _ -> state_after_flop_betting
    )
    else
      let state_for_turn = reset_player_bets_for_street state_after_flop_betting in
      let state_after_turn_deal = Game.reveal_community_cards state_for_turn 1 in
    print_endline "\n-- Turn --";
      Ui.print_game_state state_after_turn_deal dealer_button_pos;
      let starting_player_turn = get_starting_player_index (Game.get_players state_after_turn_deal) dealer_button_pos in 
      let state_after_turn_betting = betting_round state_after_turn_deal starting_player_turn min_bet_for_round in

      let active_players_turn = List.filter (fun p -> not (Player.is_folded p)) (Game.get_players state_after_turn_betting) in
      if List.length active_players_turn <= 1 then (
        match active_players_turn with
        | [winner] -> 
            Printf.printf "\n🏆 Winner as only remaining player: %s\n" (Player.get_name winner);
            let final_state = Game.distribute_pot state_after_turn_betting [Player.get_id winner] in
            Printf.printf "\n--- End of Hand ---";
            Ui.print_game_state final_state dealer_button_pos;
            final_state
        | _ -> state_after_turn_betting
      )
      else
        let state_for_river = reset_player_bets_for_street state_after_turn_betting in
        let state_after_river_deal = Game.reveal_community_cards state_for_river 1 in
    print_endline "\n-- River --";
        Ui.print_game_state state_after_river_deal dealer_button_pos;
        let starting_player_river = get_starting_player_index (Game.get_players state_after_river_deal) dealer_button_pos in 
        let state_after_river_betting = betting_round state_after_river_deal starting_player_river min_bet_for_round in
         

        let final_players = Game.get_players state_after_river_betting in
        let active_players = List.filter (fun p -> not (Player.is_folded p)) final_players in

        let winners_info = 
          match active_players with
          | [] -> []  
          | [sole_winner] -> 
              Printf.printf "\n🏆 Winner by default (others folded): %s\n" (Player.get_name sole_winner);
              [(Player.get_name sole_winner, Hand.HighCard [])]  
          | _ -> (* Showdown *)
              print_endline "\n--- Showdown ---";
              let community_cards = Game.get_community_cards state_after_river_betting in
              let contenders = List.map (fun p -> 
                let all_cards = Player.get_hand p @ community_cards in
                Printf.printf "%s shows: %s\n" 
                  (Player.get_name p) 
                  (String.concat " " (List.map Card.string_of_card (Player.get_hand p)));
                (Player.get_name p, all_cards)
              ) active_players in
              
              let winning_hands_info = Hand.best_hands contenders in
              List.iter (fun (name, hand_rank) -> 
                Printf.printf "🏆 Winner: %s with %s\n" name (Hand.string_of_hand_rank hand_rank)
              ) winning_hands_info;
              winning_hands_info
        in

        let winner_ids = 
          List.map (fun (name, _) -> 
            let player_obj = List.find (fun p -> Player.get_name p = name) final_players in
            Player.get_id player_obj
          ) winners_info 
        in
        
        let state_after_pot_distribution =
          if List.length winner_ids > 0 then
            Game.distribute_pot state_after_river_betting winner_ids
          else
            state_after_river_betting  
        in
        Printf.printf "\n--- End of Hand ---";
        Ui.print_game_state state_after_pot_distribution dealer_button_pos;  
        state_after_pot_distribution
  