open Card
open Player

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

let init_state n =
  {
    players = List.init n (fun _ -> Player.init_player);
    deck = Card.shuffle (Card.make_deck [] (0, 0));
    board = [];
    pot = 0;
    raised = 0;
  }

let raise state amount =
  {
    players = state.players;
    deck = state.deck;
    board = state.board;
    pot = state.pot + amount;
    raised = amount;
  }

let call state amount =
  {
    players = state.players;
    deck = state.deck;
    board = state.board;
    pot = state.pot + amount;
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
