open Final_project

let () = 
  print_endline "Welcome to OCaml Poker!";
  print_string "Press Enter to start the game...";
  ignore (read_line ());
   
  let player_names = ["You"; "CPU 1"; "CPU 2"] in
  let initial_chips = 1000 in
  let game_state = Game.create_game player_names initial_chips in
   
  let players, deck = Game.deal_initial_hands 
    (Game.get_players game_state) 
    (Game.get_deck game_state) in
  let game_state = { 
    game_state with 
    Game.players = players; 
    Game.deck = deck 
  } in
   
  print_endline "\nGame started! Here's the initial state:";
  Ui.print_game_state game_state 0;  
  
  print_endline "\nGame ready for play! (Add game loop next)"