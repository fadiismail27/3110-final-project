open Player
open Card

type t = {
  players : Player.t list;
  deck : Card.t list;
  pot : int;
  community_cards : Card.t list;
  current_bet : int;
}

let create_game player_names initial_chips = 
  if initial_chips <= 0 then invalid_arg "Initial chips must be positive";
  if List.length player_names < 2 then invalid_arg "Need at least 2 players";
   
  let players = List.mapi (fun id name -> 
    (Player.create_player id name initial_chips)
  ) player_names in
   
  let deck = Card.create_deck ()  in
   
  {
    players;
    deck;
    pot = 0;
    community_cards = [];
    current_bet = 0;
  }

let get_players g = g.players
let get_deck g = g.deck
let get_pot g = g.pot
let get_community_cards g = g.community_cards
let get_current_bet g = g.current_bet

let deal_initial_hands players deck = 
  if List.length players = 0 then invalid_arg "No players to deal to";
  
  let rec deal_cards remaining_players remaining_deck acc_players =
    match remaining_players with
    | [] -> (List.rev acc_players, remaining_deck)
    | player :: rest_players ->
        match remaining_deck with
        | card1 :: card2 :: new_deck ->
            let updated_player = { 
              player with 
              hand = [card1; card2]  
            } in
            deal_cards rest_players new_deck (updated_player :: acc_players)
        | _ -> failwith "Not enough cards in deck to deal"
  in
  deal_cards players deck []

  let is_game_over g =
    (* Ends the game if all players are out of chips or all but one folded *)
    let active_players = List.filter (fun p -> p.chips > 0) g.players in
    List.length active_players <= 1
  