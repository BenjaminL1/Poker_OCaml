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

let raise_action (state : state) (player_num : int) () =
  print_endline "Enter an amount to raise";
  raise state (read_int ()) player_num

let call_action (state : state) (player_num : int) () =
  let amount = state.raised in
  raise state amount player_num

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

let rec reveal_player_cards (state : state) (players : int) (iter : int) =
  if iter = players then ()
  else if (List.nth state.players iter).active then (
    print_endline ("Here are the cards for player " ^ string_of_int (iter + 1));
    print_endline (string_of_card_lst (List.nth state.players iter).cards);

    if state.raised = 0 then (
      print_endline "Raise, check, or fold?";
      match raise_check state (iter + 1) () with
      | Fold t ->
          clear ();
          print_endline "Type 'next' to reveal your cards";
          next_command ();
          reveal_player_cards t players (iter + 1)
      | Raise t ->
          clear ();
          print_endline "Type 'next' to reveal your cards";
          next_command ();
          reveal_player_cards t players (iter + 1)
      | Call t ->
          clear ();
          print_endline "Type 'next' to reveal your cards";
          next_command ();
          reveal_player_cards t players (iter + 1))
    else (
      print_endline
        ("Current bet is " ^ string_of_int state.raised
       ^ ". Raise, call, or fold?");
      match raise_call state (iter + 1) () with
      | Fold t ->
          clear ();
          print_endline "Type 'next' to reveal your cards";
          next_command ();
          reveal_player_cards t players (iter + 1)
      | Raise t ->
          clear ();
          print_endline "Type 'next' to reveal your cards";
          next_command ();
          reveal_player_cards t players (iter + 1)
      | Call t ->
          clear ();
          print_endline "Type 'next' to reveal your cards";
          next_command ();
          reveal_player_cards t players (iter + 1)))
  else reveal_player_cards state players (iter + 1)

let continue_state init_state pn = reveal_player_cards init_state pn 0

let rec startgame (pn : int) =
  match read_line () with
  | line ->
      if line = "next" then continue_state (init_state pn) pn
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
          "The Game has been initialized. Type 'next' to show the cards of \
           player 1 and 'exit' to exit";
        startgame player_num)
      else (
        print_endline "Error: player number must be between 2 and 10.";
        main ())

let () = main ()
