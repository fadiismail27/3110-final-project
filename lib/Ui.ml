open Game
open Card
open Player

let print_community_cards cc =
  print_string "Community Cards: ";
  match cc with
  | [] -> print_string "[ ] [ ] [ ]"
  | [ a ] -> Printf.printf "[ %s ] [ ] [ ]" (Card.string_of_card a)
  | [ a; b ] ->
      Printf.printf "[ %s ] [ %s ] [ ]" (Card.string_of_card a)
        (Card.string_of_card b)
  | a :: b :: c :: _ ->
      Printf.printf "[ %s ] [ %s ] [ %s ]" (Card.string_of_card a)
        (Card.string_of_card b) (Card.string_of_card c)

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
