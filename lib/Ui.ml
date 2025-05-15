open Game
open Card
open Player

[@@@coverage off]

let print_community_cards cc =
  print_string "Community Cards: ";
  let card_strings = List.map Card.string_of_card cc in
  let num_revealed = List.length card_strings in
  let num_placeholders = max 0 (5 - num_revealed) in
  let placeholders = List.init num_placeholders (fun _ -> "[ ]") in
  let display_cards =
    List.map (fun s -> "[ " ^ s ^ " ]") card_strings @ placeholders
  in
  let final_display = List.filteri (fun i _ -> i < 5) display_cards in
  print_string (String.concat " " final_display)

let print_player is_current is_show_hand idx p =
  Printf.printf "%d. %s ($%d): " idx (Player.get_name p) (Player.get_chips p);
  if Player.is_folded p then print_string "[FOLDED]"
  else if is_show_hand then
    List.iter
      (fun c -> print_string (string_of_card c ^ " "))
      (Player.get_hand p)
  else print_string "[ ?? ?? ]";
  if Player.get_current_bet p > 0 then
    Printf.printf " (bet: $%d)" (Player.get_current_bet p);
  print_newline ()

let print_game_state gs current_player_idx =
  (* clear_screen ??? *)
  Printf.printf "\n--- Pot: $%d | Current Bet to Call: $%d ---\n"
    (Game.get_pot gs) (Game.get_current_bet gs);
  print_community_cards (Game.get_community_cards gs);
  print_newline ();
  print_newline ();

  print_endline "Players:";
  List.iteri
    (fun idx p ->
      print_player (idx = current_player_idx)
        (Player.get_name p = "You")
        (idx + 1) p)
    (Game.get_players gs);
  print_newline ()

let print_action_menu gs (current_player : Player.t) _min_bet_to_open =
  Printf.printf "\nTurn for %s (Chips: $%d). Your choice:\n"
    (Player.get_name current_player)
    (Player.get_chips current_player);
  let game_current_bet = Game.get_current_bet gs in
  let player_committed_bet = Player.get_current_bet current_player in
  let can_check =
    game_current_bet = 0 || player_committed_bet = game_current_bet
  in

  if can_check then print_string "(C)heck, (B)et amount, (F)old, (A)ll-in"
  else
    let amount_to_call = game_current_bet - player_committed_bet in
    Printf.printf "(L)Call $%d, (R)aise to amount, (F)old, (A)ll-in"
      amount_to_call;

    print_string "\n> ";
    flush stdout

let rec get_bet_amount (prompt : string) (player_chips : int)
    (min_bet_val : int) (max_bet_val : int) (is_raise_action : bool)
    (current_game_bet : int) (player_committed_this_round : int) : int =
  Printf.printf "%s (min: %d, max: %d): " prompt min_bet_val max_bet_val;
  flush stdout;
  try
    let amount_input = read_int () in
    if is_raise_action then begin
      if amount_input < min_bet_val || amount_input > max_bet_val then begin
        Printf.printf "Invalid amount. Raise total must be between %d and %d.\n"
          min_bet_val max_bet_val;
        get_bet_amount prompt player_chips min_bet_val max_bet_val
          is_raise_action current_game_bet player_committed_this_round
      end
      else amount_input
    end
    else begin
      if amount_input < min_bet_val || amount_input > max_bet_val then begin
        Printf.printf "Invalid amount. Bet amount must be between %d and %d.\n"
          min_bet_val max_bet_val;
        get_bet_amount prompt player_chips min_bet_val max_bet_val
          is_raise_action current_game_bet player_committed_this_round
      end
      else amount_input
    end
  with Failure _ | End_of_file ->
    print_endline "Invalid input, please enter a number.";
    get_bet_amount prompt player_chips min_bet_val max_bet_val is_raise_action
      current_game_bet player_committed_this_round

type player_action_choice =
  | Check
  | Bet of int
  | Call
  | Raise of int
  | Fold
  | AllIn

let rec get_player_action gs (player : Player.t) (min_bet_to_open : int) :
    player_action_choice =
  let game_current_bet = Game.get_current_bet gs in
  let player_chips = Player.get_chips player in
  let player_committed_this_round = Player.get_current_bet player in
  let can_check =
    game_current_bet = 0 || player_committed_this_round = game_current_bet
  in

  match String.trim (read_line ()) |> String.lowercase_ascii with
  | "f" -> Fold
  | "a" -> AllIn
  | "c" ->
      if can_check then Check
      else (
        print_endline "Cannot check, there is a bet to call or raise.";
        get_player_action gs player min_bet_to_open)
  | "l" ->
      if not can_check then Call
      else (
        print_endline "No bet to call. You can check or bet.";
        get_player_action gs player min_bet_to_open)
  | "b" ->
      if can_check then
        let min_bet_val = min min_bet_to_open player_chips in
        let max_bet_val = player_chips in
        if max_bet_val = 0 then (
          print_endline "No chips to bet.";
          Check)
        else if min_bet_val > max_bet_val then (
          print_endline "Not enough chips for minimum bet.";
          get_player_action gs player min_bet_to_open)
        else
          Bet
            (get_bet_amount "Enter bet amount" player_chips min_bet_val
               max_bet_val false game_current_bet player_committed_this_round)
      else (
        print_endline "Cannot open bet, there is an existing bet.";
        get_player_action gs player min_bet_to_open)
  | "r" ->
      if not can_check then
        let min_increase =
          max min_bet_to_open (game_current_bet - player_committed_this_round)
        in
        let min_total_for_raise = game_current_bet + min_increase in
        let actual_min_total_for_raise =
          min min_total_for_raise (player_chips + player_committed_this_round)
        in
        let max_total_for_raise = player_chips + player_committed_this_round in

        if actual_min_total_for_raise > max_total_for_raise then (
          print_endline "Not enough chips to raise. You can call or go all-in.";
          get_player_action gs player min_bet_to_open)
        else if actual_min_total_for_raise = max_total_for_raise then (
          print_endline "Only option to raise is all-in.";
          Raise max_total_for_raise)
        else
          Raise
            (get_bet_amount "Enter new total bet amount for raise" player_chips
               actual_min_total_for_raise max_total_for_raise true
               game_current_bet player_committed_this_round)
      else (
        print_endline "Cannot raise when you can check. Please bet (B) first.";
        get_player_action gs player min_bet_to_open)
  | _ ->
      print_endline "Invalid choice, try again (f, c, b, l, r, a).";
      get_player_action gs player min_bet_to_open
