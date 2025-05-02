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
  if amount > player.chips then
    raise (Invalid_argument "Insufficient chips")
  else
    { player with chips = player.chips - amount }