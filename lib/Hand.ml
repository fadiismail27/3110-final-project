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

let rec combinations n lst =
  if n = 0 then [ [] ]
  else
    match lst with
    | [] -> []
    | x :: xs ->
        let with_x = List.map (fun l -> x :: l) (combinations (n - 1) xs) in
        let without_x = combinations n xs in
        with_x @ without_x

let rec compare_kickers k1 k2 =
  match (k1, k2) with
  | [], [] -> 0
  | x :: xs, y :: ys ->
      let cmp = compare (rank_val x) (rank_val y) in
      if cmp <> 0 then cmp else compare_kickers xs ys
  | _, _ -> failwith "Kicker lists must be the same length"

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

let hd_opt = function
  | [] -> None
  | x :: _ -> Some x

let eval_5_card_hand (cards : Card.t list) : hand_rank =
  let sorted =
    List.sort (fun h1 h2 -> compare (rank_val h2) (rank_val h1)) cards
  in
  let rank_groups = group_by_rank sorted in
  let suit_groups = group_by_suit sorted in
  let take n lst =
    List.fold_left
      (fun (acc, i) c -> if i < n then (c :: acc, i + 1) else (acc, i))
      ([], 0) lst
    |> fst |> List.rev
  in
  let is_straight (ranks : int list) =
    let deduped = List.sort_uniq compare ranks in
    let rec aux = function
      | a :: b :: c :: d :: e :: _
        when a = b + 1 && b = c + 1 && c = d + 1 && d = e + 1 -> Some a
      | _ :: t -> aux t
      | _ -> None
    in
    match aux deduped with
    | Some high -> Some high
    | None ->
        if
          List.mem 14 deduped && List.mem 13 deduped && List.mem 12 deduped
          && List.mem 11 deduped && List.mem 10 deduped
        then Some 14
        else if
          List.mem 14 deduped && List.mem 2 deduped && List.mem 3 deduped
          && List.mem 4 deduped && List.mem 5 deduped
        then Some 5
        else None
  in
  match List.find_opt (fun (_, cs) -> List.length cs >= 5) suit_groups with
  | Some (_, suited_cards) -> (
      let suited_sorted =
        List.sort (fun a b -> compare (rank_val b) (rank_val a)) suited_cards
      in
      let straight_flush =
        combinations 5 suited_sorted
        |> List.find_map (fun c ->
               let ranks = List.map rank_val c in
               match is_straight ranks with
               | Some hi -> Some (StraightFlush hi)
               | None -> None)
      in
      match straight_flush with
      | Some sf -> sf
      | None -> Flush (take 5 suited_sorted))
  | None -> (
      match rank_groups with
      | (r1, cards1) :: _ when List.length cards1 = 4 ->
          let kicker = List.find (fun c -> rank_val c <> r1) sorted in
          FourOfAKind (r1, kicker)
      | (r1, cards1) :: (r2, cards2) :: _
        when List.length cards1 = 3 && List.length cards2 >= 2 ->
          FullHouse (r1, r2)
      | _ -> (
          match is_straight (List.map rank_val sorted) with
          | Some hi -> Straight hi
          | None -> (
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

let evaluate_hand cards =
  if List.length cards <> 7 then
    failwith "evaluate_hand requires exactly 7 cards"
  else
    let five_card_combinations = combinations 5 cards in
    let rec find_best_hand hands best_hand =
      match hands with
      | [] -> best_hand
      | current_hand :: rest ->
          let current_rank = eval_5_card_hand current_hand in
          let new_best = if compare_hands current_rank best_hand > 0 then current_rank else best_hand in
          find_best_hand rest new_best
    in
    find_best_hand five_card_combinations (HighCard [])

let best_hands players =
  let evaluated_hands =
    List.map (fun (name, cards) -> (name, evaluate_hand cards)) players
  in
  let best_rank =
    List.fold_left
      (fun best (_, rank) ->
        if compare_hands rank best > 0 then rank else best)
      (HighCard []) evaluated_hands
  in
  List.filter (fun (_, rank) -> compare_hands rank best_rank = 0) evaluated_hands

let string_of_hand_rank = function
  | HighCard cards ->
      let high_card = List.hd cards in
      Printf.sprintf "High Card: %s" (Card.string_of_card high_card)
  | OnePair (rank, _) ->
      Printf.sprintf "One Pair: %s" (Card.string_of_rank (Card.int_to_rank rank))
  | TwoPair (high, low, _) ->
      Printf.sprintf "Two Pair: %s and %s"
        (Card.string_of_rank (Card.int_to_rank high))
        (Card.string_of_rank (Card.int_to_rank low))
  | ThreeOfAKind (rank, _) ->
      Printf.sprintf "Three of a Kind: %s"
        (Card.string_of_rank (Card.int_to_rank rank))
  | Straight high ->
      Printf.sprintf "Straight: %s high"
        (Card.string_of_rank (Card.int_to_rank high))
  | Flush cards ->
      Printf.sprintf "Flush: %s high"
        (Card.string_of_card (List.hd cards))
  | FullHouse (three, two) ->
      Printf.sprintf "Full House: %s over %s"
        (Card.string_of_rank (Card.int_to_rank three))
        (Card.string_of_rank (Card.int_to_rank two))
  | FourOfAKind (rank, kicker) ->
      Printf.sprintf "Four of a Kind: %s with %s kicker"
        (Card.string_of_rank (Card.int_to_rank rank))
        (Card.string_of_card kicker)
  | StraightFlush high ->
      Printf.sprintf "Straight Flush: %s high"
        (Card.string_of_rank (Card.int_to_rank high))
