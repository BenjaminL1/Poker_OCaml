open Deck
open Card
open Player
open State

let clear () = match Sys.command "clear" with _ -> ()

let rec next_command () =
  match read_line () with
  | line ->
      if line = "next" then ()
      else if line = "exit" then Stdlib.exit 0
      else (
        print_endline
          "invalid input: enter 'next' to proceed, 'exit' to exit the program";
        next_command ())

let rec raise_action (state : state) (player_num : int) () =
  print_endline "Enter an amount to raise";
  let amount = read_int () in
  if amount <= (List.nth state.players (player_num - 1)).chips then
    raise state amount player_num
  else (
    print_endline "can't raise that much!";
    raise_action state player_num ())

(* player num is 1 for player 1 *)
let call_action (state : state) (player_num : int) () =
  let amount = state.raised - (List.nth state.players (player_num - 1)).bet in
  call state amount player_num

let rec raise_call (state : state) (player_num : int) () =
  match read_line () with
  | "fold" -> Fold (fold state player_num)
  | "raise" -> Raise (raise_action state player_num ())
  | "call" -> Call (call_action state player_num ())
  | "check" ->
      print_endline "Can't check! Enter raise, call, or fold";
      raise_call state player_num ()
  | "exit" -> Stdlib.exit 0
  | _ ->
      print_endline "please input a valid action";
      raise_call state player_num ()

let rec raise_check (state : state) (player_num : int) () =
  match read_line () with
  | "fold" -> Fold (fold state player_num)
  | "raise" -> Raise (raise_action state player_num ())
  | "call" -> Call (call_action state player_num ())
  | "check" -> Call (call_action state player_num ())
  | "exit" -> Stdlib.exit 0
  | _ ->
      print_endline "please input a valid action";
      raise_check state player_num ()

let rec print_active (players : player list) (iter : int) =
  match players with
  | h :: t ->
      if h.active then (
        print_endline
          ("Player "
          ^ string_of_int (iter + 1)
          ^ ": " ^ string_of_card_lst h.cards);
        print_active t (iter + 1))
      else print_active t (iter + 1)
  | [] -> ()

let rec go_showdown (state : state) (players : int) =
  print_endline "Showdown! Here are the board and hands";
  print_endline ("Board = [" ^ string_of_board state.board ^ "]");
  print_active state.players 0;
  print_endline
    "If there is a winner, type winner, or if there's a split, type split";
  match read_line () with
  | line -> (
      match line with
      | "winner" ->
          print_endline "Type the number of the player who won:";
          let x = read_int () in
          winner state players x
      | "split" -> split_winner state players
      | _ ->
          print_endline "Please enter a valid input";
          go_showdown state players)

let description (state : state) () =
  match state.board with
  | PreFlop -> print_endline "It is currently the Preflop"
  | Flop f -> print_endline ("Board : " ^ string_of_card_lst f)
  | Turn f -> print_endline ("Board : " ^ string_of_card_lst f)
  | River f -> print_endline ("Board : " ^ string_of_card_lst f)

let rec game_loop (state : state) (players : int) (iter : int) =
  print_endline (string_of_state state);

  action state players iter

and action (state : state) (players : int) (iter : int) =
  if (List.nth state.players iter).active then (
    print_endline ("Here are the cards for player " ^ string_of_int (iter + 1));
    print_endline (string_of_card_lst (List.nth state.players iter).cards);
    description state ();
    if state.raised = 0 || (List.nth state.players iter).bet = state.raised then (
      print_endline
        ("Current bet is " ^ string_of_int state.raised
       ^ ". Raise, check, or call?");
      match raise_check state (iter + 1) () with
      | Fold t -> transition t players iter
      | Raise t -> transition t players iter
      | Call t -> transition t players iter)
    else (
      print_endline
        ("Current bet is " ^ string_of_int state.raised
       ^ ". Raise, fold, or call "
        ^ string_of_int (state.raised - (List.nth state.players iter).bet)
        ^ "?");
      match raise_call state (iter + 1) () with
      | Fold t -> transition t players iter
      | Raise t -> transition t players iter
      | Call t -> transition t players iter))
  else transition state players iter

and transition (state : state) (players : int) (iter : int) =
  clear ();
  if only_active_player state.players then (
    let won = next_active_player state players iter in
    let new_state = winner state players ((won + 1) mod players) in
    clear ();
    print_endline (string_of_state new_state);
    print_endline
      ("Player "
      ^ string_of_int ((won + 1) mod players)
      ^ " wins " ^ string_of_int state.pot ^ "!");
    print_endline "Type 'next' to reveal your cards";
    next_command ();
    game_loop new_state players ((new_state.sblind + 2) mod players))
  else (
    print_endline "Type 'next' to reveal your cards";
    next_command ());
  if round_finished_check state players iter then
    let new_state =
      match state.board with
      | PreFlop -> make_flop state players
      | Flop _ -> make_turn state players
      | Turn _ -> make_river state players
      | River _ -> go_showdown state players
    in
    if new_state.board = PreFlop then
      game_loop new_state players ((new_state.sblind + 2) mod players)
    else
      game_loop new_state players
        (next_active_player state players state.sblind)
  else
    game_loop state players
      (next_active_player state players ((iter + 1) mod players))

let rec startgame (pn : int) =
  match read_line () with
  | line ->
      if line = "next" then game_loop (init_state pn) pn (2 mod pn)
      else if line = "exit" then Stdlib.exit 0
      else (
        print_endline
          "invalid input: enter 'next' to show player one cards, 'exit' to \
           exit the program";
        startgame pn)

let rec main () =
  print_endline
    "Please enter the number of players for Texas Hold'em Poker, between 2 and \
     10: \n";
  print_string "> ";
  match read_int () with
  | exception _ ->
      print_endline "Error: player number must be between 2 and 10.";
      main ()
  | player_num ->
      if player_num = 0 then Stdlib.exit 0
      else if player_num >= 2 && player_num <= 10 then (
        print_endline ("You selected " ^ string_of_int player_num ^ " players.");
        print_endline
          ("The Game has been initialized. Type 'next' to show the cards of \
            player "
          ^ string_of_int ((2 mod player_num) + 1)
          ^ " and 'exit' to exit");
        startgame player_num)
      else (
        print_endline "Error: player number must be between 2 and 10.";
        main ())

let () = main ()
