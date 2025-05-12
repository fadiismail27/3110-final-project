open Game
open Card
open Player

let print_community_cards cc =
  print_string "Community Cards: ";
  let card_strings = List.map Card.string_of_card cc in
  let num_revealed = List.length card_strings in
  (* Ensure we don't try to create negative placeholders if somehow > 5 cards *)
  let num_placeholders = max 0 (5 - num_revealed) in 
  let placeholders = List.init num_placeholders (fun _ -> "[ ]") in
  (* Format revealed cards and append placeholders *)
  let display_cards = List.map (fun s -> "[ " ^ s ^ " ]") card_strings @ placeholders in
  (* Ensure exactly 5 slots are printed, taking only the first 5 if > 5 *)
  let final_display = List.filteri (fun i _ -> i < 5) display_cards in 
  print_string (String.concat " " final_display)

let print_player is_current is_show_hand idx p =
  Printf.printf "%d. %s ($%d): " idx (Player.get_name p) (Player.get_chips p);
  if Player.is_folded p then print_string "[FOLDED]"
  else if is_show_hand || is_current then
    List.iter
      (fun c -> print_string (string_of_card c ^ " "))
      (Player.get_hand p)
  else print_string "[ ?? ?? ]";
  if Player.get_current_bet p > 0 then
    Printf.printf " (bet: $%d)" (Player.get_current_bet p);
  print_newline ()

let print_game_state gs current_player_idx =
  (* clear_screen ??? *)
  Printf.printf "POKER - $%d Pot\n" (Game.get_pot gs);
  print_community_cards (Game.get_community_cards gs);
  print_newline ();
  print_newline ();

  print_endline "Players:";
  List.iteri
    (fun idx p -> print_player (idx = current_player_idx) false (idx + 1) p)
    (Game.get_players gs);
  print_newline ()

let print_action_menu gs current_player min_bet =
  print_endline "Your options:";
  if Game.get_current_bet gs = 0 then print_endline "[1] Check       [2] Bet"
  else Printf.printf "[1] Call ($%d)   [2] Raise\n" (Game.get_current_bet gs);

  print_endline "[3] Fold        [4] All-in";
  print_string "> "

let rec get_player_action () =
  match read_line () with
  | "1" -> `Call
  | "2" -> `Raise
  | "3" -> `Fold
  | "4" -> `AllIn
  | _ ->
      print_endline "Invalid choice, try again";
      get_player_action ()
