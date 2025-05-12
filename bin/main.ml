open ANSITerminal
open Final_project
open Round
open Game
open Ui
open Player

let horizontal_line () =
  ANSITerminal.print_string [ ANSITerminal.blue ] (String.make 40 '=' ^ "\n")

let print_banner msg =
  horizontal_line ();
  ANSITerminal.print_string [ Bold; magenta ] ("\n" ^ msg ^ "\n");
  horizontal_line (); print_newline ()

let random_cpu_names n =
  let base = ["Alice"; "Bob"; "Charlie"; "Diana"; "Eve"; "Frank"; "Grace"] in
  List.init n (fun i -> List.nth base i)

let rec ask_to_continue () =
  ANSITerminal.print_string [ yellow ] "\nWould you like to play another hand? (y/n): ";
  match String.lowercase_ascii (read_line ()) with
  | "y" -> true
  | "n" -> false
  | _ -> ANSITerminal.print_string [ red ] "Invalid input.\n"; ask_to_continue ()

let rec offer_rebuy player rebuy_amount =
  if player.chips > 0 then player else (
    ANSITerminal.print_string [ magenta ]
      (player.name ^ " is out. Buy back in or exit? (type amount or 'exit')\n");
    match read_line () with
    | "exit" -> { player with folded = true }
    | amt_str -> (
      match int_of_string_opt amt_str with
      | Some amt when amt > 0 -> { player with chips = amt; folded = false; is_all_in = false }
      | _ -> ANSITerminal.print_string [ red ] "Invalid. Defaulting to exit.\n";
             { player with folded = true }
    )
  )

let rec full_game_loop state rebuy_amount round_count win_map =
  print_banner "========= NEW ROUND =========";  (* prints \n========= NEW ROUND ========= *)
  let state = Round.play_round state in
  print_game_state state 0;

  let active = List.filter (fun p -> not p.folded) state.players in
  let hands = List.map (fun p -> (p.name, p.hand @ state.community_cards)) active in
  let winners = Hand.best_hands hands in

  let updated_win_map =
    match winners with
    | [] -> win_map
    | [name, _] ->
        if List.mem_assoc name win_map then
          List.map (fun (n, w) -> if n = name then (n, w + 1) else (n, w)) win_map
        else (name, 1) :: win_map
    | multi ->
        List.fold_left (fun acc (name, _) ->
          if List.mem_assoc name acc then
            List.map (fun (n, w) -> if n = name then (n, w + 1) else (n, w)) acc
          else (name, 1) :: acc
        ) win_map multi
  in

  if ask_to_continue () then (
    let rebought_players = List.map (fun p -> offer_rebuy p rebuy_amount) state.players in
    let still_playing = List.exists (fun p -> not (Player.is_folded p)) rebought_players in
    if still_playing then
      let new_state = {
        state with
        players = rebought_players;
        pot = 0;
        community_cards = []
      } in
      full_game_loop new_state (rebuy_amount + 200) (round_count + 1) updated_win_map
    else (
      print_banner "All previous players exited. Starting new table...";
      let cpu_count = 2 + Random.int 5 in
      let cpu_names = random_cpu_names cpu_count in
      let player_names = "You" :: cpu_names in
      let new_players = List.mapi (fun i name -> Player.create_player i name rebuy_amount) player_names in
      let new_deck = Card.create_deck () in
      let players, deck = Game.deal_initial_hands new_players new_deck in
      let new_state = Game.create_game player_names rebuy_amount in
      let new_state = { new_state with players; deck } in
      full_game_loop new_state (rebuy_amount + 200) (round_count + 1) updated_win_map
    )
  ) else (
    print_banner "Thanks for playing! Final Win Counts:";
    List.iter (fun (n, w) -> Printf.printf "%s: %d wins\n" n w) updated_win_map
  )

let () =
  Random.self_init ();
  print_banner "Welcome to the Poker Game!";
  ANSITerminal.print_string [ yellow ] "Press Enter to start the game...";
  ignore (read_line ());

  let cpu_count = 2 + Random.int 5 in
  let cpu_names = random_cpu_names cpu_count in
  let player_names = "You" :: cpu_names in

  let initial_chips = 1000 in
  let game_state = Game.create_game player_names initial_chips in

  let players, deck =
    Game.deal_initial_hands
      (Game.get_players game_state)
      (Game.get_deck game_state)
  in
  let game_state = { game_state with players; deck } in

  ANSITerminal.print_string [ green ] "\nGame started! Here's the initial state:\n";
  print_game_state game_state 0;

  print_banner "Game ready for play!";
  full_game_loop game_state initial_chips 1 []
