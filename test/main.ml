open OUnit2
open Deck.Card
open Deck.Player
open Deck.Features

let make_deck_test (name : string) (expected_output : card list) : test =
  name >:: fun _ ->
  (* the [printer] tells OUnit how to convert the output to a string *)
  assert_equal expected_output (make_deck [] (0, 0)) ~printer:string_of_card_lst

let card_tests =
  [
    make_deck_test "test deck creation"
      [
        { suit = Spade; value = Two };
        { suit = Spade; value = Three };
        { suit = Spade; value = Four };
        { suit = Spade; value = Five };
        { suit = Spade; value = Six };
        { suit = Spade; value = Seven };
        { suit = Spade; value = Eight };
        { suit = Spade; value = Nine };
        { suit = Spade; value = Ten };
        { suit = Spade; value = J };
        { suit = Spade; value = Q };
        { suit = Spade; value = K };
        { suit = Spade; value = A };
        { suit = Club; value = Two };
        { suit = Club; value = Three };
        { suit = Club; value = Four };
        { suit = Club; value = Five };
        { suit = Club; value = Six };
        { suit = Club; value = Seven };
        { suit = Club; value = Eight };
        { suit = Club; value = Nine };
        { suit = Club; value = Ten };
        { suit = Club; value = J };
        { suit = Club; value = Q };
        { suit = Club; value = K };
        { suit = Club; value = A };
        { suit = Heart; value = Two };
        { suit = Heart; value = Three };
        { suit = Heart; value = Four };
        { suit = Heart; value = Five };
        { suit = Heart; value = Six };
        { suit = Heart; value = Seven };
        { suit = Heart; value = Eight };
        { suit = Heart; value = Nine };
        { suit = Heart; value = Ten };
        { suit = Heart; value = J };
        { suit = Heart; value = Q };
        { suit = Heart; value = K };
        { suit = Heart; value = A };
        { suit = Diamond; value = Two };
        { suit = Diamond; value = Three };
        { suit = Diamond; value = Four };
        { suit = Diamond; value = Five };
        { suit = Diamond; value = Six };
        { suit = Diamond; value = Seven };
        { suit = Diamond; value = Eight };
        { suit = Diamond; value = Nine };
        { suit = Diamond; value = Ten };
        { suit = Diamond; value = J };
        { suit = Diamond; value = Q };
        { suit = Diamond; value = K };
        { suit = Diamond; value = A };
      ];
  ]

let init_deal_cards_test (name : string) (num_players : int)
    (expected_output : player list) : test =
  name >:: fun _ ->
  (* the [printer] tells OUnit how to convert the output to a string *)
  assert_equal expected_output
    (init_deal_cards [] num_players (make_deck [] (0, 0)))
    ~printer:string_of_player_list

let functions_tests =
  [
    init_deal_cards_test "2 players" 2
      [
        (* player 1 *)
        {
          cards =
            [ { suit = Spade; value = Two }; { suit = Spade; value = Three } ];
          chips = 1000;
        };
        (* player 2 *)
        {
          cards =
            [ { suit = Spade; value = Four }; { suit = Spade; value = Five } ];
          chips = 1000;
        };
      ];
    init_deal_cards_test "5 players" 5
      [
        (* player 1 *)
        {
          cards =
            [ { suit = Spade; value = Two }; { suit = Spade; value = Three } ];
          chips = 1000;
        };
        (* player 2 *)
        {
          cards =
            [ { suit = Spade; value = Four }; { suit = Spade; value = Five } ];
          chips = 1000;
        };
        (* player 3 *)
        {
          cards =
            [ { suit = Spade; value = Six }; { suit = Spade; value = Seven } ];
          chips = 1000;
        };
        (* player 4 *)
        {
          cards =
            [ { suit = Spade; value = Eight }; { suit = Spade; value = Nine } ];
          chips = 1000;
        };
        (* player 5 *)
        {
          cards = [ { suit = Spade; value = Ten }; { suit = Spade; value = J } ];
          chips = 1000;
        };
      ];
    init_deal_cards_test "10 players" 10
      [
        (* player 1 *)
        {
          cards =
            [ { suit = Spade; value = Two }; { suit = Spade; value = Three } ];
          chips = 1000;
        };
        (* player 2 *)
        {
          cards =
            [ { suit = Spade; value = Four }; { suit = Spade; value = Five } ];
          chips = 1000;
        };
        (* player 3 *)
        {
          cards =
            [ { suit = Spade; value = Six }; { suit = Spade; value = Seven } ];
          chips = 1000;
        };
        (* player 4 *)
        {
          cards =
            [ { suit = Spade; value = Eight }; { suit = Spade; value = Nine } ];
          chips = 1000;
        };
        (* player 5 *)
        {
          cards = [ { suit = Spade; value = Ten }; { suit = Spade; value = J } ];
          chips = 1000;
        };
        (* player 6 *)
        {
          cards = [ { suit = Spade; value = Q }; { suit = Spade; value = K } ];
          chips = 1000;
        };
        (* player 7 *)
        {
          cards = [ { suit = Spade; value = A }; { suit = Club; value = Two } ];
          chips = 1000;
        };
        (* player 8 *)
        {
          cards =
            [ { suit = Club; value = Three }; { suit = Club; value = Four } ];
          chips = 1000;
        };
        (* player 9 *)
        {
          cards =
            [ { suit = Club; value = Five }; { suit = Club; value = Six } ];
          chips = 1000;
        };
        (* player 10 *)
        {
          cards =
            [ { suit = Club; value = Seven }; { suit = Club; value = Eight } ];
          chips = 1000;
        };
      ];
  ]

let suite =
  "test suite for A2" >::: List.flatten [ card_tests; functions_tests ]

let _ = run_test_tt_main suite
