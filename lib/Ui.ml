open ANSITerminal
open Game
open Card
open Player

(* 🎨 Helper to color cards by suit *)
let style_of_suit = function
  | Card.Hearts | Card.Diamonds -> [ Bold; red ] (* red suits *)
  | Card.Clubs -> [ Bold; green ]
  | Card.Spades -> [ Bold; blue ]

let styled_card c =
  let txt = Card.string_of_card c in
  ANSITerminal.sprintf (style_of_suit c.suit) "[%s]" txt

let horizontal_line () =
  ANSITerminal.print_string [ Bold; cyan ]
    "=====================================================================";
  print_newline ()

(* 💠 COMMUNITY CARDS *)
let print_community_cards cc =
  horizontal_line ();
  (* top border *)
  ANSITerminal.print_string [ Bold; magenta ] "💠  COMMUNITY CARDS  💠\n";
  let card_cells =
    match cc with
    | [] -> List.init 5 (fun _ -> "[   ]")
    | cards ->
        let rendered = List.map styled_card cards in
        rendered @ List.init (5 - List.length cards) (fun _ -> "[   ]")
  in
  ANSITerminal.print_string [ Bold ] (String.concat " " card_cells);
  print_newline ();
  horizontal_line ()

(* 🧍 PLAYER ROW *)
let print_player is_current is_show_hand idx p =
  let arrow = if is_current then "➡️ " else "  " in
  let name_style = if is_current then [ Bold; yellow ] else [ Bold ] in
  let name_txt = Printf.sprintf "%s%-10s" arrow (Player.get_name p) in
  ANSITerminal.print_string name_style name_txt;
  ANSITerminal.print_string [ Bold; white ]
    (Printf.sprintf "  $%d  " (Player.get_chips p));
  (match Player.is_folded p with
  | true -> ANSITerminal.print_string [ red ] "[FOLDED]"
  | false ->
      let hand_txt =
        Player.get_hand p |> List.map styled_card |> String.concat " "
      in
      let hidden = "[ ?? ?? ]" in
      ANSITerminal.print_string [ default ]
        (if is_show_hand || is_current then hand_txt else hidden));
  if Player.get_current_bet p > 0 then
    ANSITerminal.print_string [ cyan ]
      (Printf.sprintf "  (bet: $%d)" (Player.get_current_bet p));
  print_newline ()

let print_game_state gs current_idx =
  horizontal_line ();
  ANSITerminal.print_string [ Bold; yellow ]
    (Printf.sprintf "💰 POT: $%d\n" (Game.get_pot gs));
  print_community_cards (Game.get_community_cards gs);
  ANSITerminal.print_string [ Bold; magenta ] "\n🎴 PLAYERS:\n";
  List.iteri
    (fun i p -> print_player (i = current_idx) false (i + 1) p)
    (Game.get_players gs);
  horizontal_line ();
  print_newline ()

(* ACTION MENU *)
let print_action_menu gs =
  ANSITerminal.print_string [ Bold; green ] "\n📋 Your options:\n";
  if Game.get_current_bet gs = 0 then
    ANSITerminal.print_string [ green ] "[1] Check       [2] Bet\n"
  else
    ANSITerminal.print_string [ green ]
      (Printf.sprintf "[1] Call ($%d)   [2] Raise\n" (Game.get_current_bet gs));
  ANSITerminal.print_string [ green ] "[3] Fold        [4] All-in\n";
  ANSITerminal.print_string [ yellow ] "> "

let rec get_player_action () =
  match read_line () with
  | "1" -> `Call
  | "2" -> `Raise
  | "3" -> `Fold
  | "4" -> `AllIn
  | _ ->
      ANSITerminal.print_string [ red ] "❌ Invalid choice, try again\n";
      get_player_action ()

let print_intro () =
  horizontal_line ();
  ANSITerminal.print_string [ Bold; magenta ]
    "♠️ ♥️  Welcome to OCaml Terminal Poker!  ♣️ ♦️\n";
  ANSITerminal.print_string [ Bold; cyan ]
    "Play hands, raise bets, and fold with style!\n";
  horizontal_line ();
  print_newline ()

let print_flop_header () =
  ANSITerminal.print_string [ Bold; magenta ]
    "\n========= NEW ROUND =========\n"

let print_stage_header name =
  ANSITerminal.print_string [ Bold; white ] ("\n--- " ^ name ^ " ---\n")

let print_prompt prompt = ANSITerminal.print_string [ yellow ] prompt
