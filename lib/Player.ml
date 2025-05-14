open Card

type t = {
  id : int;
  name : string;
  chips : int;
  hand : Card.t list;
  folded : bool;
  current_bet : int;
  is_all_in : bool;
}

let create_player id name chips =
  if chips < 0 then invalid_arg "Chips cannot be negative";
  {
    id;
    name;
    chips;
    hand = [];
    folded = false;
    current_bet = 0;
    is_all_in = false;
  }

let get_id p = p.id
let get_name p = p.name
let get_chips p = p.chips
let get_hand p = p.hand
let is_folded p = p.folded
let get_current_bet p = p.current_bet
let set_current_bet p amount = { p with current_bet = amount }

let is_all_in p = p.is_all_in
let set_all_in p status = { p with is_all_in = status }

let set_folded player status = { player with folded = status }
let update_chips player change = { player with chips = player.chips + change }

let reset_for_new_hand player =
  { player with 
    hand = []; 
    folded = false; 
    current_bet = 0; 
    is_all_in = false 
  }

let fold player = set_folded player true

let bet player amount =
  if amount < 0 then raise (Invalid_argument "Bet amount cannot be negative") else
  if amount > player.chips then 
    { player with chips = 0; current_bet = player.current_bet + player.chips; is_all_in = true }
  else if amount = player.chips then 
     { player with chips = 0; current_bet = player.current_bet + amount; is_all_in = true }
  else
    { player with chips = player.chips - amount; current_bet = player.current_bet + amount }

let call player game_current_bet =
  let amount_to_call = game_current_bet - player.current_bet in
  if amount_to_call <= 0 then player  
  else if amount_to_call >= player.chips then  
    { player with chips = 0; current_bet = player.current_bet + player.chips; is_all_in = true }
  else
    { player with chips = player.chips - amount_to_call; current_bet = player.current_bet + amount_to_call }

let check player game_current_bet =
  if player.current_bet < game_current_bet then
    raise (Invalid_argument "Cannot check when there is a bet to call")
  else
    player  

let raise_bet player new_total_bet =
  let amount_to_raise = new_total_bet - player.current_bet in
  if amount_to_raise <= 0 then raise (Invalid_argument "Raise amount must be greater than current bet contribution") else
  if new_total_bet > player.chips + player.current_bet then  
    { player with chips = 0; current_bet = player.current_bet + player.chips; is_all_in = true }
  else if new_total_bet = player.chips + player.current_bet then  
     { player with chips = 0; current_bet = player.current_bet + player.chips; is_all_in = true }
  else
    { player with chips = player.chips - amount_to_raise; current_bet = player.current_bet + amount_to_raise }