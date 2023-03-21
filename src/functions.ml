open Card
open Player

exception EmptyDeck

let rec deal_cards (acc : player list) (num_players : int) (deck : card list) :
    player list =
  if num_players > 0 then
    deal_cards
      (acc @ [ [ List.hd deck; List.hd (List.tl deck) ] ])
      (num_players - 1)
      (List.tl (List.tl deck))
  else acc
