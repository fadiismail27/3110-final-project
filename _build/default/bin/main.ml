open Final_project
open Round
open Game
open Ui
open Player
let rec game_loop state =
  if Game.is_game_over state then begin
    print_endline "Game over!";
    match List.find_opt (fun p -> p.chips > 0) (Game.get_players state) with
    | Some winner ->
        print_endline ("🏆 Final winner: " ^ winner.name ^ " with " ^ string_of_int winner.chips ^ " chips!")
    | None ->
        print_endline "No players have chips. It's a draw!"
  end
  else
    let new_state = Round.play_round state in
    Ui.print_game_state new_state 0;
    game_loop new_state

let () =
  print_endline "Welcome to OCaml Poker!";
  print_string "Press Enter\n   to start the game...";
  ignore (read_line ());

  let player_names = [ "You"; "CPU 1"; "CPU 2" ] in
  let initial_chips = 1000 in
  let game_state = Game.create_game player_names initial_chips in

  let players, deck =
    Game.deal_initial_hands
      (Game.get_players game_state)
      (Game.get_deck game_state)
  in
  let game_state = { game_state with players; deck } in

  print_endline "\nGame started! Here's the initial state:";
  Ui.print_game_state game_state 0;

  print_endline "\nGame ready for play!";
  game_loop game_state