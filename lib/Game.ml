open Player
open Card

type t = {
  players : Player.t list;
  deck : Card.t list;
  pot : int;
  community_cards : Card.t list;
  current_bet : int;
}
 
let update_player_in_list players updated_player =
  List.map (fun p ->
    if Player.get_id p = Player.get_id updated_player then updated_player else p
  ) players

let rec take n lst = if n <= 0 then [] else match lst with | [] -> [] | x::xs -> x :: take (n-1) xs
let rec drop n lst = if n <= 0 then lst else match lst with | [] -> [] | _::xs -> drop (n-1) xs

 
let shuffle_deck (gs : t) : t =
  let deck_list = gs.deck in
  let deck_array = Array.of_list deck_list in
  let n = Array.length deck_array in 
  for i = n - 1 downto 1 do
    let j = Random.int (i + 1) in  
    let temp = deck_array.(i) in
    deck_array.(i) <- deck_array.(j);
    deck_array.(j) <- temp
  done;
  { gs with deck = Array.to_list deck_array }
  
(* Game *)
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

(* Action *)

let handle_player_fold (gs: t) (player_id: int) : t =
  let player = List.find (fun p -> Player.get_id p = player_id) gs.players in
  let folded_player = Player.fold player in
  { gs with players = update_player_in_list gs.players folded_player }

let handle_player_check (gs: t) (player_id: int) : t =
  let player = List.find (fun p -> Player.get_id p = player_id) gs.players in
  let checked_player = Player.check player gs.current_bet in  
  { gs with players = update_player_in_list gs.players checked_player }


let handle_player_bet (gs: t) (player_id: int) (bet_amount: int) : t =
  let player = List.find (fun p -> Player.get_id p = player_id) gs.players in
  let chips_player_had = Player.get_chips player in
  let amount_actually_bet =  
    let current_player_bet_in_round = Player.get_current_bet player in
    let intended_total_for_player_this_round = current_player_bet_in_round + bet_amount in
    if intended_total_for_player_this_round >= chips_player_had + current_player_bet_in_round then  
      chips_player_had
    else
      bet_amount
  in
  let updated_player = Player.bet player bet_amount in  
  let new_pot = gs.pot + amount_actually_bet in
  let new_game_current_bet = Player.get_current_bet updated_player in 
  { gs with 
    players = update_player_in_list gs.players updated_player; 
    pot = new_pot; 
    current_bet = new_game_current_bet
  }

let handle_player_call (gs: t) (player_id: int) : t =
  let player = List.find (fun p -> Player.get_id p = player_id) gs.players in
  let bet_to_match = gs.current_bet in
  let player_current_contribution = Player.get_current_bet player in
  let chips_player_has = Player.get_chips player in 
  let amount_needed_to_call = bet_to_match - player_current_contribution in

  let amount_to_add_to_pot = 
    if amount_needed_to_call <= 0 then 0 
    else if amount_needed_to_call >= chips_player_has then chips_player_has
    else amount_needed_to_call
  in 
  let updated_player = Player.call player bet_to_match in  
  let new_pot = gs.pot + amount_to_add_to_pot in
  { gs with 
    players = update_player_in_list gs.players updated_player; 
    pot = new_pot 
  }

let handle_player_raise (gs: t) (player_id: int) (new_total_bet_for_round: int) : t = 
  let player = List.find (fun p -> Player.get_id p = player_id) gs.players in
  let player_current_contribution = Player.get_current_bet player in
  let chips_player_has = Player.get_chips player in
   
  let additional_chips_for_raise = new_total_bet_for_round - player_current_contribution in

  let amount_to_add_to_pot =
    if additional_chips_for_raise >= chips_player_has then chips_player_has  
    else additional_chips_for_raise
  in
  let updated_player = Player.raise_bet player new_total_bet_for_round in  
  let new_pot = gs.pot + amount_to_add_to_pot in
  { gs with 
    players = update_player_in_list gs.players updated_player; 
    pot = new_pot; 
    current_bet = new_total_bet_for_round
  }
 
  let deal_initial_hands players deck =
    if players = [] then
      invalid_arg "No players to deal to"
    else if List.length deck < List.length players * 2 then
      failwith "Not enough cards to deal initial hands"
    else
      let rec deal acc_players cards_to_deal remaining_deck =
        match cards_to_deal with
        | [] -> (List.rev acc_players, remaining_deck)
        | p :: ps ->
            let hand = take 2 remaining_deck in
            let new_deck = drop 2 remaining_deck in
            let updated_player = { p with hand = hand } in
            deal (updated_player :: acc_players) ps new_deck
      in
      deal [] players deck
  
 
let new_hand (gs: t) : t =
  let reset_players = List.map Player.reset_for_new_hand gs.players in
  let initial_state = { players = reset_players; pot = 0; community_cards = []; current_bet = 0; deck = Card.create_deck () } in
  let shuffled_state = shuffle_deck initial_state in
  let (players_with_hands, remaining_deck) = deal_initial_hands shuffled_state.players shuffled_state.deck in
  { shuffled_state with players = players_with_hands; deck = remaining_deck }
 
  let reveal_community_cards (gs: t) (num_cards: int) : t =
  if num_cards <= 0 then gs else
  let cards_to_reveal = take num_cards gs.deck in
  let remaining_deck = drop num_cards gs.deck in
  { gs with
    community_cards = gs.community_cards @ cards_to_reveal;
    deck = remaining_deck;
    current_bet = 0;  
    players = List.map (fun p -> Player.set_current_bet p 0) gs.players; 
     }
 
let is_game_over g =
  let players_with_chips = List.filter (fun p -> Player.get_chips p > 0) g.players in
  List.length players_with_chips <= 1
 
let distribute_pot (gs: t) (winner_ids: int list) : t =
  if List.length winner_ids = 0 then gs  
  else
    let total_pot = gs.pot in
    let num_winners = List.length winner_ids in
    let winnings_per_winner = total_pot / num_winners in  
    
    let updated_players = List.map (fun p ->
      if List.mem (Player.get_id p) winner_ids then
        Player.update_chips p winnings_per_winner
      else
        p
    ) gs.players in
    
    { gs with players = updated_players; pot = 0 }  
  