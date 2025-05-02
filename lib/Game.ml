open Player
open Card

type t = {
  players : Player.t list;
  deck : Card.t list;
  pot : int;
  community_cards : Card.t list;
  current_bet : int;
}

(* --- Helper --- *)
let update_player_in_list players updated_player =
  List.map (fun p ->
    if Player.get_id p = Player.get_id updated_player then updated_player else p
  ) players

let rec take n lst = if n <= 0 then [] else match lst with | [] -> [] | x::xs -> x :: take (n-1) xs
let rec drop n lst = if n <= 0 then lst else match lst with | [] -> [] | _::xs -> drop (n-1) xs

(* --- Shuffle --- *)
let shuffle_deck (gs : t) : t =
  let deck_list = gs.deck in
  let deck_array = Array.of_list deck_list in
  let n = Array.length deck_array in
  (* Fisher-Yates shuffle *)
  for i = n - 1 downto 1 do
    let j = Random.int (i + 1) in (* 0 <= j <= i *)
    let temp = deck_array.(i) in
    deck_array.(i) <- deck_array.(j);
    deck_array.(j) <- temp
  done;
  { gs with deck = Array.to_list deck_array }
  
(* --- Create Game --- *)
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

(* --- Getters --- *)
let get_players g = g.players
let get_deck g = g.deck
let get_pot g = g.pot
let get_community_cards g = g.community_cards
let get_current_bet g = g.current_bet

(* --- Deal Initial Hands --- *)
let deal_initial_hands players deck =
  let num_players = List.length players in
  if List.length deck < num_players * 2 then failwith "Not enough cards to deal initial hands" else
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

(* --- New Hand Setup --- *)
let new_hand (gs: t) : t =
  let reset_players = List.map Player.reset_for_new_hand gs.players in
  let initial_state = { players = reset_players; pot = 0; community_cards = []; current_bet = 0; deck = Card.create_deck () } in
  let shuffled_state = shuffle_deck initial_state in
  let (players_with_hands, remaining_deck) = deal_initial_hands shuffled_state.players shuffled_state.deck in
  { shuffled_state with players = players_with_hands; deck = remaining_deck }

(* --- Reveal Community Cards --- *)
let reveal_community_cards (gs: t) (num_cards: int) : t =
  if num_cards <= 0 then gs else
  let cards_to_reveal = take num_cards gs.deck in
  let remaining_deck = drop num_cards gs.deck in
  { gs with
    community_cards = gs.community_cards @ cards_to_reveal;
    deck = remaining_deck;
    current_bet = 0; (* Reset bet amount for the new street *)
    (* Reset player current bets - keep for consistency, though not used yet *)
    players = List.map (fun p -> Player.set_current_bet p 0) gs.players; 
     }

(* --- Game Over Check --- *)
let is_game_over g =
  let players_with_chips = List.filter (fun p -> Player.get_chips p > 0) g.players in
  List.length players_with_chips <= 1
  