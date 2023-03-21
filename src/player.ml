open Card

type player = card list

let rec string_of_player_lst_helper (lst : player list) : string =
  match lst with
  | h :: t ->
      "\n" ^ string_of_card_lst h ^ "; \n" ^ string_of_player_lst_helper t
  | [] -> ""

let string_of_player_list (lst : player list) : string =
  "\n[" ^ string_of_player_lst_helper lst ^ "]\n"
