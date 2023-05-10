open Card
open Player
(* open Features *)

(* exception Too_many_cards *)

(* state of the board, the board is a card option list
   of length 5 initialized with all None*)
(* All functions here have the precondition that there are enough cards left
   in the deck to play with, if this isn't true, refresh the deck and state *)
type state = {
  players : player list;
  deck : card list;
  board : card option list;
  pot : int;
  raised : int;
}

type action = Raise of state | Call of state | Fold of state

let init_state n =
  let shuf_deck = Card.shuffle (Card.make_deck [] (0, 0)) in
  {
    players = Features.init_deal_cards [] n shuf_deck;
    deck = shuf_deck;
    board = [];
    pot = 0;
    raised = 0;
  }

let rec round_finished_check (players : player list) (amount : int) =
  match players with
  | [] -> true
  | h :: t -> if h.bet <> amount then false else round_finished_check t amount

(* player_num starts at 1 for the first player in the list *)
let rec player_bet (players : player list) (amount : int) (player_num : int) =
  match players with
  | [] -> failwith "not a player"
  | h :: t when player_num <> 1 -> h :: player_bet t amount (player_num - 1)
  | h :: t when player_num = 1 ->
      {
        cards = h.cards;
        chips = h.chips - amount;
        bet = amount;
        active = h.active;
      }
      :: t
  | _ -> failwith "player_bet input error"

let raise state amount player_num =
  {
    players = player_bet state.players amount player_num;
    deck = state.deck;
    board = state.board;
    pot = state.pot + amount;
    raised = amount;
  }

let call state amount player_num =
  {
    players = player_bet state.players amount player_num;
    deck = state.deck;
    board = state.board;
    pot = state.pot + amount;
    raised = amount;
  }

let rec fold_helper (players : player list) (player_num : int) =
  match players with
  | [] -> failwith "player_num too high"
  | h :: t when player_num <> 1 -> h :: fold_helper t (player_num - 1)
  | h :: t when player_num = 1 ->
      { cards = h.cards; chips = h.chips; bet = h.bet; active = not h.active }
      :: t
  | _ -> failwith "player_bet input error"

let fold (state : state) (player_num : int) =
  {
    players = fold_helper state.players player_num;
    deck = state.deck;
    board = state.board;
    pot = state.pot;
    raised = 0;
  }

let rec flip_board (board : card option list) card =
  match board with
  | [] -> []
  | None :: t -> Some card :: t
  | Some s :: t -> Some s :: flip_board t card

(* Transfers next card from deck to board *)
let flip state =
  {
    players = state.players;
    deck = List.tl state.deck;
    board = flip_board state.board (List.hd state.deck);
    pot = state.pot;
    raised = state.raised;
  }
