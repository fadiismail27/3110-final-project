open Card

type hand_rank =
  | HighCard of Card.t list
  | OnePair of int * Card.t list
  | TwoPair of int * int * Card.t list
  | ThreeOfAKind of int * Card.t list
  | Straight of int
  | Flush of Card.t list
  | FullHouse of int * int
  | FourOfAKind of int * Card.t
  | StraightFlush of int

let rank_val c =
  match c.rank with
  | Two -> 2
  | Three -> 3
  | Four -> 4
  | Five -> 5
  | Six -> 6
  | Seven -> 7
  | Eight -> 8
  | Nine -> 9
  | Ten -> 10
  | Jack -> 11
  | Queen -> 12
  | King -> 13
  | Ace -> 14

(** [hand_rank_strength h] returns an integer representing the relative strength
    of the poker hand [h], with higher numbers indicating stronger hands. This
    is used to compare hands of different types, such as Flush vs Straight. *)
let hand_rank_strength = function
  | HighCard _ -> 0
  | OnePair _ -> 1
  | TwoPair _ -> 2
  | ThreeOfAKind _ -> 3
  | Straight _ -> 4
  | Flush _ -> 5
  | FullHouse _ -> 6
  | FourOfAKind _ -> 7
  | StraightFlush _ -> 8

(* helper to generate all combinations of n elements from a list *)
let rec combinations n lst =
  if n = 0 then [ [] ]
  else
    match lst with
    | [] -> []
    | x :: xs ->
        let with_x = List.map (fun l -> x :: l) (combinations (n - 1) xs) in
        let without_x = combinations n xs in
        with_x @ without_x

(** [compare_kickers k1 k2] compares two lists of kicker cards [k1] and [k2],
    assuming both are sorted in descending order by rank. It returns:
    - 1 if [k1] is stronger
    - -1 if [k2] is stronger
    - 0 if all corresponding cards are equal. Requires: [k1] and [k2] must be
      the same length. *)
let rec compare_kickers k1 k2 =
  match (k1, k2) with
  | [], [] -> 0
  | x :: xs, y :: ys ->
      let cmp = compare (rank_val x) (rank_val y) in
      if cmp <> 0 then cmp else compare_kickers xs ys
  | _, _ -> failwith "Kicker lists must be the same length"

(* placeholder comparison for hand ranks — implement later *)
let compare_hands h1 h2 =
  match (h1, h2) with
  | StraightFlush r1, StraightFlush r2 -> compare r1 r2
  | FourOfAKind (r1, k1), FourOfAKind (r2, k2) ->
      let cmp = compare r1 r2 in
      if cmp <> 0 then cmp else compare (rank_val k1) (rank_val k2)
  | FullHouse (t1, p1), FullHouse (t2, p2) ->
      let cmp = compare t1 t2 in
      if cmp <> 0 then cmp else compare p1 p2
  | Flush cards1, Flush cards2 ->
      let rec compare_cards c1 c2 =
        match (c1, c2) with
        | [], [] -> 0
        | x :: xs, y :: ys ->
            let cmp = compare (rank_val x) (rank_val y) in
            if cmp <> 0 then cmp else compare_cards xs ys
        | _, _ -> failwith "Invalid flush comparison"
      in
      compare_cards cards1 cards2
  | Straight r1, Straight r2 -> compare r1 r2
  | ThreeOfAKind (r1, ks1), ThreeOfAKind (r2, ks2) ->
      let cmp = compare r1 r2 in
      if cmp <> 0 then cmp else compare_kickers ks1 ks2
  | TwoPair (h1, l1, k1), TwoPair (h2, l2, k2) ->
      let cmp_high = compare h1 h2 in
      if cmp_high <> 0 then cmp_high
      else
        let cmp_low = compare l1 l2 in
        if cmp_low <> 0 then cmp_low else compare_kickers k1 k2
  | OnePair (r1, ks1), OnePair (r2, ks2) ->
      let cmp = compare r1 r2 in
      if cmp <> 0 then cmp else compare_kickers ks1 ks2
  | HighCard cs1, HighCard cs2 -> compare_kickers cs1 cs2
  | h1, h2 -> compare (hand_rank_strength h1) (hand_rank_strength h2)

(** [group_by_rank cards] returns a list of (rank, card list) pairs, where each
    pair represents all cards in [cards] that share the same rank. The list is
    sorted by decreasing group size, with ties broken by higher rank values. *)
let group_by_rank cards =
  let tbl = Hashtbl.create 5 in
  List.iter
    (fun card ->
      let r = rank_val card in
      let prev = Hashtbl.find_opt tbl r |> Option.value ~default:[] in
      Hashtbl.replace tbl r (card :: prev))
    cards;
  Hashtbl.to_seq tbl |> List.of_seq
  |> List.sort (fun (r1, l1) (r2, l2) ->
         let len_cmp = compare (List.length l2) (List.length l1) in
         if len_cmp <> 0 then len_cmp else compare r2 r1)

(** [group_by_suit cards] returns a list of (suit, card list) pairs, where each
    pair represents all cards in [cards] that share the same suit. The list is
    sorted by decreasing group size, with ties broken by higher suit values. *)
let group_by_suit cards =
  let tbl = Hashtbl.create 5 in
  List.iter
    (fun card ->
      let s = card.suit in
      let prev = Hashtbl.find_opt tbl s |> Option.value ~default:[] in
      Hashtbl.replace tbl s (card :: prev))
    cards;
  Hashtbl.to_seq tbl |> List.of_seq
  |> List.sort (fun (r1, l1) (r2, l2) ->
         let len_cmp = compare (List.length l2) (List.length l1) in
         if len_cmp <> 0 then len_cmp else compare r2 r1)

(* placeholder evaluator for a single 5-card hand — implement later *)
let eval_5_card_hand (cards : Card.t list) : hand_rank =
  let sorted =
    List.sort (fun h1 h2 -> compare (rank_val h2) (rank_val h1)) cards
  in
  let rank_groups = group_by_rank sorted in
  let suit_groups = group_by_suit sorted in
  (* Helper to get top N sorted cards *)
  let take n lst =
    List.fold_left
      (fun (acc, i) c -> if i < n then (c :: acc, i + 1) else (acc, i))
      ([], 0) lst
    |> fst |> List.rev
  in

  (* STRAIGHT or STRAIGHT FLUSH check *)
  let is_straight (ranks : int list) =
    let deduped = List.sort_uniq compare ranks in
    let rec aux = function
      | a :: b :: c :: d :: e :: _
        when a = b + 1 && b = c + 1 && c = d + 1 && d = e + 1 -> Some a
      | _ :: t -> aux t
      | _ -> None
    in
    (* Ace-low straight special case *)
    match aux deduped with
    | Some high -> Some high
    | None ->
        if
          List.mem 14 deduped && List.mem 2 deduped && List.mem 3 deduped
          && List.mem 4 deduped && List.mem 5 deduped
        then Some 5
        else None
  in

  (* 1. Straight Flush *)
  match List.find_opt (fun (_, cs) -> List.length cs >= 5) suit_groups with
  | Some (_, suited_cards) -> (
      let suited_sorted =
        List.sort (fun a b -> compare (rank_val b) (rank_val a)) suited_cards
      in
      let straight_flush_high =
        suited_sorted |> List.map rank_val |> is_straight
      in
      match straight_flush_high with
      | Some hi -> StraightFlush hi
      | None ->
          (* 4. Flush *)
          Flush (take 5 suited_sorted))
  | None -> (
      (* 2. Four of a Kind *)
      match rank_groups with
      | (r1, cards1) :: rest when List.length cards1 = 4 ->
          let kicker = List.find (fun c -> rank_val c <> r1) sorted in
          FourOfAKind (r1, kicker)
      | (r1, cards1) :: (r2, cards2) :: _
        when List.length cards1 = 3 && List.length cards2 >= 2 ->
          (* 3. Full House *)
          FullHouse (r1, r2)
      | _ -> (
          (* 5. Straight *)
          match is_straight (List.map rank_val sorted) with
          | Some hi -> Straight hi
          | None -> (
              (* Check other hand types using rank_groups *)
              match rank_groups with
              | (r1, l1) :: (r2, l2) :: (r3, l3) :: _
                when List.length l1 = 3 && List.length l2 = 2 ->
                  FullHouse (r1, r2)
              | (r1, l1) :: _ when List.length l1 = 3 ->
                  let kickers =
                    List.filter (fun c -> rank_val c <> r1) sorted |> take 2
                  in
                  ThreeOfAKind (r1, kickers)
              | (r1, l1) :: (r2, l2) :: _
                when List.length l1 = 2 && List.length l2 = 2 ->
                  let kicker =
                    List.find
                      (fun c ->
                        let rc = rank_val c in
                        rc <> r1 && rc <> r2)
                      sorted
                  in
                  let high, low = if r1 > r2 then (r1, r2) else (r2, r1) in
                  TwoPair (high, low, [ kicker ])
              | (r1, l1) :: _ when List.length l1 = 2 ->
                  let kickers =
                    List.filter (fun c -> rank_val c <> r1) sorted |> take 3
                  in
                  OnePair (r1, kickers)
              | _ -> HighCard (take 5 sorted))))

let evaluate_hand (cards : Card.t list) : hand_rank =
  let hands = combinations 5 cards in
  List.fold_left
    (fun acc h ->
      let r = eval_5_card_hand h in
      match compare_hands r acc with
      | 1 -> r
      | _ -> acc)
    (HighCard []) hands

(** [best_hands players] takes a list of (name, 7-card hand) pairs and returns
    all players with the strongest evaluated hand. *)
let best_hands (players : (string * Card.t list) list) :
    (string * hand_rank) list =
  let ranked =
    List.map (fun (name, cards) -> (name, evaluate_hand cards)) players
  in
  let best =
    List.fold_left
      (fun acc (n, h) ->
        match acc with
        | [] -> [ (n, h) ]
        | (_, best_hand) :: _ as all ->
            let cmp = compare_hands h best_hand in
            if cmp > 0 then [ (n, h) ]
            else if cmp = 0 then (n, h) :: all
            else all)
      [] ranked
  in
  List.rev best

let string_of_hand_rank = function
  | HighCard cards ->
      let top = List.map rank_val cards in
      "High Card: " ^ String.concat ", " (List.map string_of_int top)
  | OnePair (r, _) -> "One Pair of " ^ string_of_int r ^ "s"
  | TwoPair (h, l, _) ->
      "Two Pair: " ^ string_of_int h ^ "s and " ^ string_of_int l ^ "s"
  | ThreeOfAKind (r, _) -> "Three of a Kind: " ^ string_of_int r ^ "s"
  | Straight r -> "Straight to " ^ string_of_int r
  | Flush cards ->
      let top = List.map rank_val cards in
      "Flush: " ^ String.concat ", " (List.map string_of_int top)
  | FullHouse (t, p) ->
      "Full House: " ^ string_of_int t ^ "s full of " ^ string_of_int p ^ "s"
  | FourOfAKind (r, _) -> "Four of a Kind: " ^ string_of_int r ^ "s"
  | StraightFlush r -> "Straight Flush to " ^ string_of_int r
