let rec main () =
  print_endline
    "Please enter the number of players for Texas Hold'em Poker, between 2 and \
     10: \n";
  print_string "> ";
  let player_num = read_int () in
  if player_num >= 2 && player_num <= 10 then
    print_string ("You selected " ^ string_of_int player_num ^ " players.\n\n")
  else (
    print_string "Error: player number must be between 2 and 10.\n";
    main ())

let () = main ()
