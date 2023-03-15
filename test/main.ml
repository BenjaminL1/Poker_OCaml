open OUnit2
open Deck.Card

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

let suite = "test suite for A2" >::: List.flatten [ card_tests ]
let _ = run_test_tt_main suite
