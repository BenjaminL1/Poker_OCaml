open Deck
open Card
open Player
open State

let clear () = match Sys.command "clear" with _ -> ()

let rec pass_player () =
  match read_line () with
  | line ->
      if line = "next" then clear ()
      else if line = "exit" then Stdlib.exit 0
      else (
        print_endline
          "invalid input: enter 'next' to proceed, 'exit' to exit the program";
        pass_player ())

let rec next_command () =
  match read_line () with
  | line ->
      if line = "next" then ()
      else if line = "exit" then Stdlib.exit 0
      else (
        print_endline
          "invalid input: enter 'next' to proceed, 'exit' to exit the program";
        next_command ())

let rec reveal_player_cards (p_list : player list) (pn : int) org_pn =
  if pn = 0 then ()
  else if pn = 1 then (
    print_endline
      ("Here are the cards for player " ^ string_of_int (org_pn - pn + 1));
    print_endline (string_of_card_lst (List.hd p_list).cards))
  else (
    print_endline
      ("Here are the cards for player " ^ string_of_int (org_pn - pn + 1));
    print_endline (string_of_card_lst (List.hd p_list).cards);
    print_endline "Type 'next' to clear screen and pass to next player";
    pass_player ();
    print_endline "Type 'next' to reveal your cards";
    next_command ();
    reveal_player_cards (List.tl p_list) (pn - 1) org_pn)

let continue_state init_state pn = reveal_player_cards init_state.players pn pn

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
     10, and 0 to exit: \n";
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
