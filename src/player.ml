open Card

type player = { cards : card list; chips : int }

let rec string_of_player_lst_helper (lst : player list) : string =
  match lst with
  | h :: t ->
      "\n" ^ string_of_card_lst h.cards ^ "; \n" ^ string_of_player_lst_helper t
  | [] -> ""

let string_of_player_list (lst : player list) : string =
  "\n[" ^ string_of_player_lst_helper lst ^ "]\n"
