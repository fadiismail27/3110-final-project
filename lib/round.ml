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
  open Player
  
  let reset_folds players =
    List.map (fun p -> { p with folded = false }) players
  
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
  
    let active_players = List.filter (fun p -> not p.folded) updated_players in
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
          if p.id = winner.id then { p with chips = p.chips + 100 }
          else p)
        updated_players
    in
  
    let updated_players = reset_folds updated_players in
    { state with players = updated_players }
  