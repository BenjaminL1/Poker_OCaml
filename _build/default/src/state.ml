open Card
open Player
(* open Features *)

(* exception Too_many_cards *)

(* state of the board, the board is a card option list
   of length 5 initialized with all None*)
(* All functions here have the precondition that there are enough cards left
   in the deck to play with, if this isn't true, refresh the deck and state *)

type board =
  | PreFlop
  | Flop of card list
  | Turn of card list
  | River of card list

let bigBlind = 20

type state = {
  players : player list;
  deck : card list;
  board : board;
  pot : int;
  raised : int;
  bblind : int;
  sblind : int;
  last_raised : int;
  round : int;
}

type action = Raise of state | Call of state | Fold of state

let rec set_blinds (state : state) (playerlist : player list) (players : int)
    (iter : int) =
  match playerlist with
  | h :: t when iter = state.sblind ->
      {
        cards = h.cards;
        chips = h.chips - bigBlind;
        bet = bigBlind;
        active = h.active;
      }
      :: set_blinds state t players (iter + 1)
  | h :: t when iter = state.sblind ->
      {
        cards = h.cards;
        chips = h.chips - (bigBlind / 2);
        bet = bigBlind / 2;
        active = h.active;
      }
      :: set_blinds state t players (iter + 1)
  | h :: t -> h :: set_blinds state t players (iter + 1)
  | [] -> []

let init_state n =
  let shuf_deck = Card.shuffle (Card.make_deck [] (0, 0)) in
  {
    players = Features.init_deal_cards [] n shuf_deck;
    deck = shuf_deck;
    board = PreFlop;
    pot = 0;
    raised = 0;
    bblind = 1;
    sblind = 0;
    last_raised = 1;
    round = 1;
  }

let round_finished_check (state : state) (players : int) (iter : int) =
  if state.last_raised = (iter + 1) mod players then true else false

(* iter is player after current player who just made a move *)
let rec next_active_player (state : state) (players : int) (iter : int) =
  if (List.nth state.players (iter mod players)).active then iter
  else next_active_player state players ((iter + 1) mod players)

(* player_num is 0 for player 1 *)
let rec only_active_player (players : player list) (player_num : int)
    (iter : int) =
  match players with
  | [] -> true
  | h :: _ when iter <> player_num ->
      if h.active then false
      else only_active_player players player_num (iter + 1)
  | _ -> only_active_player players player_num (iter + 1)

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
    bblind = state.bblind;
    sblind = state.sblind;
    last_raised = player_num - 1;
    round = state.round;
  }

let call state amount player_num =
  {
    players = player_bet state.players amount player_num;
    deck = state.deck;
    board = state.board;
    pot = state.pot + amount;
    raised = amount;
    bblind = state.bblind;
    sblind = state.sblind;
    last_raised = state.last_raised;
    round = state.round;
  }

let rec fold_helper (players : player list) (player_num : int) =
  match players with
  | [] -> failwith "player_num too high"
  | h :: t when player_num <> 1 -> h :: fold_helper t (player_num - 1)
  | h :: t when player_num = 1 ->
      { cards = h.cards; chips = h.chips; bet = 0; active = false } :: t
  | _ -> failwith "player_bet input error"

let fold (state : state) (player_num : int) =
  {
    players = fold_helper state.players player_num;
    deck = state.deck;
    board = state.board;
    pot = state.pot;
    raised = state.raised;
    bblind = state.bblind;
    sblind = state.sblind;
    last_raised = state.last_raised;
    round = state.round;
  }

let rec make_flop_helper (deck : card list) (iter : int) =
  match iter with
  | 3 -> []
  | _ -> List.hd deck :: make_flop_helper (List.tl deck) (iter + 1)

let make_flop (state : state) =
  {
    players = state.players;
    deck = List.tl (List.tl (List.tl state.deck));
    board = Flop (make_flop_helper state.deck 0);
    pot = state.pot;
    raised = 0;
    bblind = state.bblind;
    sblind = state.sblind;
    last_raised = state.sblind;
    round = state.round;
  }

let make_turn_helper (deck : card list) (board : board) =
  match board with
  | Flop f -> Turn (List.hd deck :: f)
  | _ -> failwith "can't turn if not flop before"

let make_turn (state : state) =
  {
    players = state.players;
    deck = List.tl state.deck;
    board = make_turn_helper state.deck state.board;
    pot = state.pot;
    raised = 0;
    bblind = state.bblind;
    sblind = state.sblind;
    last_raised = state.sblind;
    round = state.round;
  }

let make_river_helper (deck : card list) (board : board) =
  match board with
  | Turn t -> Turn (List.hd deck :: t)
  | _ -> failwith "can't river if not turn before"

let make_river (state : state) =
  {
    players = state.players;
    deck = List.tl state.deck;
    board = make_river_helper state.deck state.board;
    pot = state.pot;
    raised = 0;
    bblind = state.bblind;
    sblind = state.sblind;
    last_raised = state.sblind;
    round = state.round;
  }

let showdown (state : state) = state

let rec winner_helper (players : player list) (amount : int) (player_num : int)
    =
  match players with
  | [] -> []
  | h :: t when player_num <> 1 ->
      { cards = h.cards; chips = h.chips; bet = 0; active = true }
      :: player_bet t amount (player_num - 1)
  | h :: t when player_num = 1 ->
      { cards = h.cards; chips = h.chips + amount; bet = 0; active = true }
      :: winner_helper t amount player_num
  | _ -> failwith "player_bet input error"

let winner (state : state) (players : int) (player_num : int) =
  let new_state =
    {
      players = winner_helper state.players state.pot player_num;
      deck = Card.shuffle_mult (Card.make_deck [] (0, 0)) (state.round + 1);
      board = make_river_helper state.deck state.board;
      pot = state.pot;
      raised = 0;
      bblind = (state.bblind + 1) mod players;
      sblind = (state.sblind + 1) mod players;
      last_raised = state.sblind;
      round = state.round + 1;
    }
  in
  {
    players = set_blinds new_state new_state.players players 0;
    deck = Card.shuffle_mult (Card.make_deck [] (0, 0)) (state.round + 1);
    board = make_river_helper state.deck state.board;
    pot = state.pot;
    raised = 0;
    bblind = (state.bblind + 1) mod players;
    sblind = (state.sblind + 1) mod players;
    last_raised = state.sblind;
    round = state.round + 1;
  }
