(* open Game
open Player

let play_round (state : Game.t) : Game.t =
  print_endline "\nStarting a new round...";

  let players = Game.get_players state in

  let updated_players =
    List.map
      (fun p ->
        if Player.is_folded p || Player.is_all_in p then p
        else if Player.get_name p = "You" then (
          print_endline "Your move. Type 'check' or 'fold': ";
          match read_line () with
          | "fold" -> { p with folded = true }
          | _ -> p (* treat anything else as check *))
        else
          let move = if Random.bool () then "fold" else "check" in
          print_endline (p.name ^ " chooses to " ^ move);
          if move = "fold" then { p with folded = true } else p)
      players
  in

  let active_players = List.filter (fun p -> not p.folded) updated_players in
  let winner =
    match active_players with
    | [] -> List.hd updated_players
    | h :: _ -> h
  in

  print_endline ("Winner: " ^ winner.name);
  let updated_players =
    List.map
      (fun p ->
        if p.id = winner.id then { p with chips = p.chips + 100 } else p)
      updated_players
  in

  { state with players = updated_players } *)
  open Game
  open Ui
  
  let reset_folds (players : Player.t list) =
    List.map (fun p -> Player.set_folded p false) players
  
  let play_round (state : Game.t) : Game.t =
    print_endline "\nStarting a new round...";
  
    let players = Game.get_players state in
  
    let updated_players =
      List.map (fun p ->
        if Player.is_folded p || Player.is_all_in p then p
        else if Player.get_name p = "You" then (
          let rec get_valid_move () =
            print_endline "Your move. Type 'check' or 'fold': ";
            match read_line () with
            | "check" -> false
            | "fold" -> true
            | _ ->
                print_endline "Invalid move.";
                get_valid_move ()
          in
          let folded = get_valid_move () in
          { p with folded }
        ) else
          let move = if Random.bool () then "fold" else "check" in
          print_endline (p.name ^ " chooses to " ^ move);
          if move = "fold" then { p with folded = true } else p
      ) players
    in
  
    let active_players = List.filter (fun p -> Player.is_folded p = false) updated_players in
    let winner =
      match active_players with
      | [] -> List.hd updated_players
      | _ ->
          let idx = Random.int (List.length active_players) in
          List.nth active_players idx
    in
  
    print_endline ("🏆 Winner this round: " ^ winner.name);
  
    let updated_players =
      List.map
        (fun p ->
          if Player.get_id p = Player.get_id winner then Player.update_chips p 100 (* Use getter and updater *)
          else p)
        updated_players
    in
  
    let updated_players = reset_folds updated_players in
    { state with players = updated_players }
  
  let play_hand_stages (state : Game.t) : Game.t =
    print_endline "\n=====================";
    print_endline "   Starting New Hand ";
    print_endline "=====================";
  
    (* 1. Prepare for new hand (shuffle, deal, reset state) *)
    let hand_state = Game.new_hand state in 
    print_endline "-- Pre-Flop --";
    Ui.print_game_state hand_state 0; (* Show player hands and initial state *)
    print_string "Press Enter to deal the Flop...";
    ignore (read_line ());
  
    (* 2. Deal Flop *)
    let state_after_flop_deal = Game.reveal_community_cards hand_state 3 in
    print_endline "\n-- Flop --";
    Ui.print_game_state state_after_flop_deal 0; (* Show flop cards *)
     print_string "Press Enter to deal the Turn...";
     ignore (read_line ());
  
    (* 3. Deal Turn *)
    let state_after_turn_deal = Game.reveal_community_cards state_after_flop_deal 1 in
    print_endline "\n-- Turn --";
    Ui.print_game_state state_after_turn_deal 0; (* Show turn card *)
    print_string "Press Enter to deal the River...";
    ignore (read_line ());
    
    (* 4. Deal River *)
    let state_after_river_deal = Game.reveal_community_cards state_after_turn_deal 1 in
    print_endline "\n-- River --";
    Ui.print_game_state state_after_river_deal 0; (* Show river card *)
    
    (* No betting, no winner determination yet *)
    print_endline "\nHand Stages Complete.";
  
    (* Return the state after the river *)
    state_after_river_deal 
  