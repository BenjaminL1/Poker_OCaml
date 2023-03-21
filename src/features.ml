open Card
open Player

exception EmptyDeck

let rec init_deal_cards (acc : player list) (num_players : int)
    (deck : card list) : player list =
  if num_players > 0 then
    init_deal_cards
      (acc
      @ [ { cards = [ List.hd deck; List.hd (List.tl deck) ]; chips = 1000 } ])
      (num_players - 1)
      (List.tl (List.tl deck))
  else acc

let rec deal (players : player list) (acc : player list) (num_players : int)
    (deck : card list) : player list =
  if num_players > 0 then
    deal (List.tl players)
      (acc
      @ [
          {
            cards = [ List.hd deck; List.hd (List.tl deck) ];
            chips = Player.num_chips (List.hd players);
          };
        ])
      (num_players - 1)
      (List.tl (List.tl deck))
  else acc
