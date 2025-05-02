open Final_project
open Game
open Ui
open Player

let rec get_num_bots () =
  print_string "Enter the number of bot players (minimum 1): ";
  flush stdout;
  match int_of_string_opt (read_line ()) with
  | Some n when n >= 1 -> n
  | _ -> 
      print_endline "Invalid input. Please enter a number greater than or equal to 1.";
      get_num_bots ()

let rec game_loop current_state =
  (* Check game over conditions *)
  let players = Game.get_players current_state in
  let human_player_opt = List.find_opt (fun p -> Player.get_name p = "You") players in
  
  match human_player_opt with
  | Some human when Player.get_chips human <= 0 ->
      print_endline "\n=============================";
      print_endline " You ran out of chips! Game Over.";
      print_endline "=============================";
      () (* End game *)
  | _ when Game.is_game_over current_state -> 
       print_endline "\n=============================";
       print_endline "      G A M E   O V E R      ";
       print_endline "=============================";
       let players_with_chips = List.filter (fun p -> Player.get_chips p > 0) players in
       (match players_with_chips with
        | [winner] -> Printf.printf "🏆 Final winner: %s with $%d chips!\n" (Player.get_name winner) (Player.get_chips winner)
        | _ -> print_endline "Game ended: Only one player left."
       );
      () (* End game *)
  | _ -> (* Continue playing *)
      (* Play one hand through its stages *)
      let state_after_hand = Round.play_hand_stages current_state in

      (* Pause and wait for Enter before starting the next hand *)
      print_string "\nPress Enter to start the next hand...";
      flush stdout; (* Ensure prompt shows before waiting *)
      ignore (read_line ());

      (* Loop for the next hand *)
      game_loop state_after_hand

let () =
  Random.self_init (); 
  print_endline "======================";
  print_endline " Welcome to OCaml Poker!";
  print_endline "======================";

  let num_bots = get_num_bots () in
  let player_names = "You" :: List.init num_bots (fun i -> "CPU " ^ string_of_int (i + 1)) in
  let initial_chips = 1000 in
  
  Printf.printf "\nStarting game with %d players (%s) and $%d initial chips.\n" 
    (List.length player_names) (String.concat ", " player_names) initial_chips;
  
  let initial_game_state = Game.create_game player_names initial_chips in

  print_string "Press Enter to start the first hand...";
  ignore (read_line ());
  
  (* Start the main game loop *)
  game_loop initial_game_state