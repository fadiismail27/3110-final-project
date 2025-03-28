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

let get_name p = p.name
let get_chips p = p.chips
let is_folded p = p.folded
let is_all_in p = p.is_all_in
let get_hand p = p.hand
let get_current_bet p = p.current_bet
let get_id p = p.id
