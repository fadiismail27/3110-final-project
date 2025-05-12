open ANSITerminal
open Game
open Player
open Card
open Hand

let draw_cards n deck =
  let rec aux n deck acc =
    if n = 0 then (List.rev acc, deck)
    else match deck with
      | [] -> failwith "Deck exhausted"
      | h::t -> aux (n - 1) t (h :: acc)
  in
  aux n deck []

let print_stage name cards =
  ANSITerminal.print_string [ Bold; white ] ("\n--- " ^ name ^ " ---\n");
  List.iter (fun c ->
    let style = match c.suit with
      | Hearts | Diamonds -> [ red; Bold ]
      | Clubs -> [ green; Bold ]
      | Spades -> [ blue; Bold ]
    in
    ANSITerminal.print_string style (Card.string_of_card c ^ " ")) cards;
  print_newline ()

let cpu_decision current_bet p =
  if p.chips <= 0 then "fold"
  else
    let moves =
      if current_bet = 0 then ["check"; "raise"; "fold"] else ["call"; "raise"; "fold"]
    in
    List.nth moves (Random.int (List.length moves))

let prompt_player p current_bet =
  ANSITerminal.print_string [ yellow ] ("Your chips: $" ^ string_of_int p.chips ^ "\n");
  if current_bet = 0 then
    ANSITerminal.print_string [ green ] "Choose: check (0), raise [type amount], all-in (a), or fold (f)\n"
  else
    ANSITerminal.print_string [ green ] (Printf.sprintf "Current bet: $%d. Choose: call (c), raise [amount], all-in (a), or fold (f)\n" current_bet);
  let input = read_line () in
  match input with
  | "0" | "check" -> ("check", 0)
  | "f" | "fold" -> ("fold", 0)
  | "c" | "call" -> ("call", current_bet - p.current_bet)
  | "a" | "allin" -> ("raise", p.chips)
  | amt_str ->
      try let amt = int_of_string amt_str in
        if amt <= 0 then ("check", 0) else ("raise", amt)
      with _ -> ANSITerminal.print_string [ red ] "Invalid. Defaulting to check.\n"; ("check", 0)

let apply_move p move amount current_bet =
  match move with
  | "fold" -> fold p, 0
  | "check" -> p, 0
  | "call" | "raise" ->
      let to_pay = min amount p.chips in
      let is_all_in = to_pay = p.chips in
      let p' = { (bet p to_pay) with current_bet = p.current_bet + to_pay; is_all_in } in
      p', to_pay
  | _ -> p, 0

let betting_round players init_bet =
  let rec loop players pot current_bet =
    let active = List.filter (fun p -> not p.folded && not p.is_all_in && p.chips > 0) players in
    if active = [] then players, pot
    else
      let rec turn players acted pot current_bet =
        match players with
        | [] -> if acted then loop players pot current_bet else players, pot
        | p::ps when p.folded || p.is_all_in || p.chips = 0 -> turn ps acted pot current_bet
        | p::ps ->
            let move, amt =
              if p.name = "You" then prompt_player p current_bet
              else
                let mv = cpu_decision current_bet p in
                let raise_amt = if p.chips <= 50 then p.chips else (Random.int (min 100 p.chips)) + 1 in
                ANSITerminal.print_string [ cyan ] (p.name ^ " chooses to " ^ mv ^ "\n");
                let a = match mv with
                  | "raise" -> raise_amt
                  | "call" -> current_bet - p.current_bet
                  | _ -> 0
                in
                mv, a
            in
            let p', to_pot = apply_move p move amt current_bet in
            let new_cb =
              if move = "raise" then p'.current_bet else current_bet
            in
            turn (ps @ [p']) true (pot + to_pot) new_cb
      in
      turn players false pot current_bet
  in
  loop players 0 init_bet

let reset_folds players = List.map (fun p -> { p with folded = false }) players

let deal_and_bet label n state =
  let drawn, new_deck = draw_cards n state.deck in
  let community = state.community_cards @ drawn in
  print_stage label drawn;
  let players', added_pot = betting_round state.players state.current_bet in
  let reset = List.map (fun p -> { p with current_bet = 0 }) players' in
  { deck = new_deck; community_cards = community; players = reset; pot = state.pot + added_pot; current_bet = 0 }

let play_round state =
  let state = deal_and_bet "Flop" 3 state in
  let state = deal_and_bet "Turn" 1 state in
  let state = deal_and_bet "River" 1 state in
  let active = List.filter (fun p -> not p.folded) state.players in
  match active with
  | [winner] ->
      ANSITerminal.print_string [ green ] ("\n🏆 All others folded! " ^ winner.name ^ " wins the pot.\n");
      let updated = List.map (fun p -> if p.name = winner.name then { p with chips = p.chips + state.pot } else p) state.players in
      { state with players = reset_folds updated; pot = 0 }
  | [] -> ANSITerminal.print_string [ red ] "No active players. Pot carries over.\n"; state
  | _ ->
      let hands = List.map (fun p -> (p.name, p.hand @ state.community_cards)) active in
      let winners = Hand.best_hands hands in
      match winners with
      | [] -> ANSITerminal.print_string [ red ] "No active players. Pot carries over.\n"; state
      | [name, rank] ->
          ANSITerminal.print_string [ green ]
            ("\n🏆 Winner: " ^ name ^ " with " ^ string_of_hand_rank rank ^ "\n");
          let updated = List.map (fun p -> if p.name = name then { p with chips = p.chips + state.pot } else p) state.players in
          { state with players = reset_folds updated; pot = 0 }
      | multi ->
          ANSITerminal.print_string [ yellow ] "\n🤝 It's a tie!\n";
          List.iter (fun (n, r) -> ANSITerminal.print_string [ magenta ] (n ^ " with " ^ string_of_hand_rank r ^ "\n")) multi;
          let share = state.pot / List.length multi in
          let winners = List.map fst multi in
          let updated = List.map (fun p -> if List.mem p.name winners then { p with chips = p.chips + share } else p) state.players in
          { state with players = reset_folds updated; pot = 0 }