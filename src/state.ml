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

(* Bigblind value *)
let bigBlind = 20

(* state type used by main *)
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

(* action type which defines which stage the hand is in *)
type action = Raise of state | Call of state | Fold of state

let string_of_board (board : board) =
  match board with
  | PreFlop -> "Preflop"
  | Flop f -> string_of_card_lst f
  | Turn t -> string_of_card_lst t
  | River r -> string_of_card_lst r

let string_of_state (state : state) =
  "players: "
  ^ string_of_player_list state.players
  ^ ", \n" ^ "deck: "
  ^ string_of_card_lst state.deck
  ^ ", \n" ^ "board: "
  ^ string_of_board state.board
  ^ ", \n" ^ "pot: " ^ string_of_int state.pot ^ ", \n" ^ "raised: "
  ^ string_of_int state.raised ^ ", \n" ^ "bblind: "
  ^ string_of_int state.bblind ^ ", \n" ^ "sblind: "
  ^ string_of_int state.sblind ^ ", \n" ^ "last_raised: "
  ^ string_of_int state.last_raised
  ^ ", \n" ^ "round: " ^ string_of_int state.round

(* given state, list of players and number of players with iter starting at 0 set_blinds
   iterates through the player list, changing the blinds of the player when they match
   the player number of state.sblind or state.bblind *)
let rec set_blinds (state : state) (playerlist : player list) (players : int)
    (iter : int) =
  match playerlist with
  | h :: t when iter = state.bblind ->
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

let rec num_active (players : player list) (counter : int) =
  match players with
  | [] -> counter
  | h :: t ->
      if h.active then num_active t (counter + 1) else num_active t counter

let init_state n =
  let shuf_state =
    let shuf_deck = Card.shuffle (Card.make_deck [] (0, 0)) in
    {
      players = Features.init_deal_cards [] n shuf_deck;
      deck = discard shuf_deck (2 * n);
      board = PreFlop;
      pot = 30;
      raised = 20;
      bblind = 1;
      sblind = 0;
      last_raised = 2 mod n;
      round = 1;
    }
  in
  {
    players = set_blinds shuf_state shuf_state.players n 0;
    deck = shuf_state.deck;
    board = PreFlop;
    pot = 30;
    raised = 20;
    bblind = 1;
    sblind = 0;
    last_raised = 2 mod n;
    round = 1;
  }

(* checks if last to act is the person before last raised *)

(* returns 0 for player 1 etc *)
let rec next_active_helper (players : player list) (player : player)
    (iter : int) =
  match players with
  | h :: t -> if h = player then iter else next_active_helper t player (iter + 1)
  | [] -> failwith "not a player"

(* iter is player after current player who just made a move *)
let rec next_active_player (state : state) (players : int) (iter : int) =
  if (List.nth state.players (iter mod players)).active then
    next_active_helper state.players
      (List.nth state.players (iter mod players))
      0
  else next_active_player state players ((iter + 1) mod players)

let round_finished_check (state : state) (players : int) (iter : int) =
  if state.board = PreFlop then
    if state.last_raised = (iter + 1) mod players then true else false
  else if
    state.last_raised = next_active_player state players ((iter + 1) mod players)
  then true
  else false

let rec only_helper (players : player list) =
  match players with
  | h :: t -> if h.active then false else only_helper t
  | [] -> true

(* player_num is 0 for player 1 *)
let rec only_active_player (players : player list) =
  match players with
  | h :: t -> if h.active then only_helper t else only_active_player t
  | [] -> failwith "no active players"

(* next active player for start of the round *)
let rec find_action_starts (state : state) (players : int) (iter : int) =
  if (List.nth state.players iter).active then iter
  else find_action_starts state players ((iter + 1) mod players)

(* changes the player info to match the bet given parameters player list amount
   and player num
      player_num starts at 1 for the first player in the list *)
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

let rec player_call (state : state) (players : player list) (amount : int)
    (player_num : int) =
  match players with
  | [] -> failwith "not a player"
  | h :: t when player_num <> 1 ->
      h :: player_call state t amount (player_num - 1)
  | h :: t when player_num = 1 ->
      {
        cards = h.cards;
        chips = h.chips - amount;
        bet = state.raised;
        active = h.active;
      }
      :: t
  | _ -> failwith "player_bet input error"

(* raise action *)
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

(* called when player calls *)
let call state amount player_num =
  {
    players = player_call state state.players amount player_num;
    deck = state.deck;
    board = state.board;
    pot = state.pot + amount;
    raised = state.raised;
    bblind = state.bblind;
    sblind = state.sblind;
    last_raised = state.last_raised;
    round = state.round;
  }

(* iterates through playerlist player_num times then folds the player at that num *)
let rec fold_helper (players : player list) (player_num : int) =
  match players with
  | [] -> failwith "player_num too high"
  | h :: t when player_num <> 1 -> h :: fold_helper t (player_num - 1)
  | h :: t when player_num = 1 ->
      { cards = h.cards; chips = h.chips; bet = 0; active = false } :: t
  | _ -> failwith "player_bet input error"

(* called when player folds *)
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

(* resets bets at the start of a new round *)
let rec reset_bets (players : player list) =
  match players with
  | h :: t ->
      { cards = h.cards; chips = h.chips; bet = 0; active = h.active }
      :: reset_bets t
  | [] -> []

let rec make_flop_helper (deck : card list) (iter : int) =
  match iter with
  | 3 -> []
  | _ -> List.hd deck :: make_flop_helper (List.tl deck) (iter + 1)

let make_flop (state : state) (players : int) =
  {
    players = reset_bets state.players;
    deck = List.tl (List.tl (List.tl state.deck));
    board = Flop (make_flop_helper state.deck 0);
    pot = state.pot;
    raised = 0;
    bblind = state.bblind;
    sblind = state.sblind;
    last_raised = next_active_player state players state.sblind;
    round = state.round;
  }

let make_turn_helper (deck : card list) (board : board) =
  match board with
  | Flop f -> Turn (List.hd deck :: f)
  | _ -> failwith "can't turn if not flop before"

let make_turn (state : state) (players : int) =
  {
    players = reset_bets state.players;
    deck = List.tl state.deck;
    board = make_turn_helper state.deck state.board;
    pot = state.pot;
    raised = 0;
    bblind = state.bblind;
    sblind = state.sblind;
    last_raised = next_active_player state players state.sblind;
    round = state.round;
  }

let make_river_helper (deck : card list) (board : board) =
  match board with
  | Turn t -> River (List.hd deck :: t)
  | _ -> failwith "can't river if not turn before"

let make_river (state : state) (players : int) =
  {
    players = reset_bets state.players;
    deck = List.tl state.deck;
    board = make_river_helper state.deck state.board;
    pot = state.pot;
    raised = 0;
    bblind = state.bblind;
    sblind = state.sblind;
    last_raised = next_active_player state players state.sblind;
    round = state.round;
  }

let showdown (state : state) = state

let rec winner_helper (players : player list) (amount : int) (won : player) =
  match players with
  | [] -> []
  | h :: t when h <> won ->
      { cards = h.cards; chips = h.chips; bet = 0; active = true }
      :: winner_helper t amount won
  | h :: t when h = won ->
      { cards = h.cards; chips = h.chips + amount; bet = 0; active = true }
      :: winner_helper t amount won
  | _ -> failwith "player_bet input error"

let rec split_helper (players : player list) (amount : int) (tied : int) =
  match players with
  | [] -> []
  | h :: t ->
      if h.active then
        {
          cards = h.cards;
          chips = h.chips + (amount / tied);
          bet = 0;
          active = true;
        }
        :: split_helper t amount tied
      else
        { cards = h.cards; chips = h.chips; bet = 0; active = true }
        :: split_helper t amount tied

let winner (state : state) (players : int) (player_num : int) =
  let new_state =
    {
      players =
        winner_helper state.players state.pot
          (List.nth state.players (player_num - 1));
      deck = make_deck [] (0, 0) |> shuffle;
      board = PreFlop;
      pot = state.pot;
      raised = 0;
      bblind = (state.bblind + 1) mod players;
      sblind = (state.sblind + 1) mod players;
      last_raised = (state.bblind + 1) mod players;
      round = state.round + 1;
    }
  in
  {
    players = set_blinds new_state new_state.players players 0;
    deck = new_state.deck;
    board = new_state.board;
    pot = new_state.pot;
    raised = bigBlind;
    bblind = new_state.bblind;
    sblind = new_state.sblind;
    last_raised = new_state.bblind;
    round = state.round;
  }

let split_winner (state : state) (players : int) =
  let new_state =
    {
      players = split_helper state.players players (num_active state.players 0);
      deck = make_deck [] (0, 0) |> shuffle;
      board = PreFlop;
      pot = state.pot;
      raised = 0;
      bblind = (state.bblind + 1) mod players;
      sblind = (state.sblind + 1) mod players;
      last_raised = (state.bblind + 1) mod players;
      round = state.round + 1;
    }
  in
  {
    players = set_blinds new_state new_state.players players 0;
    deck = new_state.deck;
    board = new_state.board;
    pot = new_state.pot;
    raised = bigBlind;
    bblind = new_state.bblind;
    sblind = new_state.sblind;
    last_raised = new_state.bblind;
    round = state.round;
  }
