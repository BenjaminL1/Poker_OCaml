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
      else if player_num >= 2 && player_num <= 10 then
        print_endline ("You selected " ^ string_of_int player_num ^ " players.")
      else (
        print_endline "Error: player number must be between 2 and 10.";
        main ())

let () = main ()
