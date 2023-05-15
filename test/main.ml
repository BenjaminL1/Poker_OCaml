(* Our test plan was to test each of the basic functions that we developed for
    our poker game. Test cases were developed using both black box and glass box
    strategies. For each functionality, we wrote test cases and slowly built our
   way up, starting from the deck and leading to each function of poker. We
   utilized OUnit tests to cover the basic tests of our game, and the remainder
   of the bugs were sorted out by simply playing games of poker. In theory, our
   test plan would eliminate most bugs, since by ensuring basic functionality,
   and running many test games to ensure that more complex functionality works,
   our system would be correct. However, there are certainly some bugs that we
   were not able to resolve. *)

open OUnit2
open Deck.Card
open Deck.Player
open Deck.Features
open Deck.State

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
    ( "shuffled deck not equal" >:: fun _ ->
      assert_equal false
        (shuffle (make_deck [] (0, 0))
        = [
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
          ]) );
    ( "two shuffled decks not equal" >:: fun _ ->
      assert_equal false
        (shuffle (make_deck [] (0, 0)) = shuffle (make_deck [] (0, 0))) );
    ( "shuffle deck length 52" >:: fun _ ->
      assert_equal 52 (List.length (shuffle (make_deck [] (0, 0)))) );
    ( "mult shuffle" >:: fun _ ->
      assert_equal false
        (shuffle_mult (make_deck [] (0, 0)) 5 = shuffle (make_deck [] (0, 0)))
    );
  ]

let deck_tests =
  [
    ( "discard" >:: fun _ ->
      assert_equal 51 (List.length (discard (make_deck [] (0, 0)) 1)) );
    ( "discard" >:: fun _ ->
      assert_equal 46
        (List.length (discard (discard (make_deck [] (0, 0)) 1) 5)) );
    ( "discard" >:: fun _ ->
      assert_equal 20
        (List.length (discard (discard (make_deck [] (0, 0)) 1) 31)) );
    ( "discard" >:: fun _ ->
      assert_equal 45
        (List.length (discard (discard (make_deck [] (0, 0)) 1) 6)) );
    ( "discard" >:: fun _ ->
      assert_equal 50 (List.length (discard (make_deck [] (0, 0)) 2)) );
    ( "discard" >:: fun _ ->
      assert_equal 49 (List.length (discard (make_deck [] (0, 0)) 3)) );
    ( "discard" >:: fun _ ->
      assert_equal 48 (List.length (discard (make_deck [] (0, 0)) 4)) );
    ( "discard" >:: fun _ ->
      assert_equal 45 (List.length (discard (make_deck [] (0, 0)) 7)) );
    ( "discard" >:: fun _ ->
      assert_equal 40 (List.length (discard (make_deck [] (0, 0)) 12)) );
    ( "discard" >:: fun _ ->
      assert_equal 35 (List.length (discard (make_deck [] (0, 0)) 17)) );
    ( "discard" >:: fun _ ->
      assert_equal 30 (List.length (discard (make_deck [] (0, 0)) 22)) );
    ( "discard" >:: fun _ ->
      assert_equal 29 (List.length (discard (make_deck [] (0, 0)) 23)) );
    ( "discard" >:: fun _ ->
      assert_equal 28 (List.length (discard (make_deck [] (0, 0)) 24)) );
    ( "discard" >:: fun _ ->
      assert_equal 27 (List.length (discard (make_deck [] (0, 0)) 25)) );
    ( "discard" >:: fun _ ->
      assert_equal 26 (List.length (discard (make_deck [] (0, 0)) 26)) );
    ( "discard" >:: fun _ ->
      assert_equal 25 (List.length (discard (make_deck [] (0, 0)) 27)) );
    ( "discard" >:: fun _ ->
      assert_equal 24 (List.length (discard (make_deck [] (0, 0)) 28)) );
    ( "discard" >:: fun _ ->
      assert_equal 23 (List.length (discard (make_deck [] (0, 0)) 29)) );
    ( "discard" >:: fun _ ->
      assert_equal 22 (List.length (discard (make_deck [] (0, 0)) 30)) );
    ( "discard" >:: fun _ ->
      assert_equal 21 (List.length (discard (make_deck [] (0, 0)) 31)) );
    ( "discard" >:: fun _ ->
      assert_equal 20 (List.length (discard (make_deck [] (0, 0)) 32)) );
    ( "discard" >:: fun _ ->
      assert_equal 19 (List.length (discard (make_deck [] (0, 0)) 33)) );
    ( "discard" >:: fun _ ->
      assert_equal 18 (List.length (discard (make_deck [] (0, 0)) 34)) );
    ( "discard" >:: fun _ ->
      assert_equal 17 (List.length (discard (make_deck [] (0, 0)) 35)) );
    ( "discard" >:: fun _ ->
      assert_equal 16 (List.length (discard (make_deck [] (0, 0)) 36)) );
    ( "discard" >:: fun _ ->
      assert_equal 15 (List.length (discard (make_deck [] (0, 0)) 37)) );
    ( "discard" >:: fun _ ->
      assert_equal 14 (List.length (discard (make_deck [] (0, 0)) 38)) );
    ( "discard" >:: fun _ ->
      assert_equal 13 (List.length (discard (make_deck [] (0, 0)) 39)) );
    ( "discard" >:: fun _ ->
      assert_equal 12 (List.length (discard (make_deck [] (0, 0)) 40)) );
    ( "discard" >:: fun _ ->
      assert_equal 11 (List.length (discard (make_deck [] (0, 0)) 41)) );
    ( "discard" >:: fun _ ->
      assert_equal 10 (List.length (discard (make_deck [] (0, 0)) 42)) );
  ]

let init_deal_cards_test (name : string) (num_players : int)
    (expected_output : player list) : test =
  name >:: fun _ ->
  (* the [printer] tells OUnit how to convert the output to a string *)
  assert_equal expected_output
    (init_deal_cards [] num_players (make_deck [] (0, 0)))
    ~printer:string_of_player_list

let player_bet_test (name : string) (state : state) (amount : int)
    (player_num : int) (expected_output : player list) : test =
  name >:: fun _ ->
  assert_equal expected_output
    (player_bet state.players amount player_num)
    ~printer:string_of_player_list

let raise_test (name : string) (state : state) (amount : int) (player_num : int)
    (expected_output : state) : test =
  name >:: fun _ ->
  assert_equal expected_output
    (raise state amount player_num)
    ~printer:string_of_state

let call_test (name : string) (state : state) (amount : int) (player_num : int)
    (expected_output : state) : test =
  name >:: fun _ ->
  assert_equal expected_output
    (call state amount player_num)
    ~printer:string_of_state

let blind_test (name : string) (state : state) (players : int)
    (expected_output : player list) : test =
  name >:: fun _ ->
  assert_equal expected_output
    (set_blinds state state.players players 0)
    ~printer:string_of_player_list

let make_flop_test (name : string) (state : state) (players : int)
    (expected_output : state) : test =
  name >:: fun _ ->
  assert_equal expected_output (make_flop state players)
    ~printer:string_of_state

let make_turn_test (name : string) (state : state) (players : int)
    (expected_output : state) : test =
  name >:: fun _ ->
  assert_equal expected_output (make_turn state players)
    ~printer:string_of_state

(* let state1 = init_state 3 *)
let phys_deck = make_deck [] (0, 0)

let blind_less_players =
  [
    {
      cards = [ { suit = Spade; value = Two }; { suit = Heart; value = Ten } ];
      chips = 1000;
      bet = 0;
      active = true;
    };
    {
      cards = [ { suit = Heart; value = Four }; { suit = Heart; value = Nine } ];
      chips = 1000;
      bet = 0;
      active = true;
    };
    {
      cards = [ { suit = Diamond; value = A }; { suit = Club; value = A } ];
      chips = 1000;
      bet = 0;
      active = true;
    };
  ]

let phys_player_list =
  [
    {
      cards = [ { suit = Spade; value = Two }; { suit = Heart; value = Ten } ];
      chips = 990;
      bet = 10;
      active = true;
    };
    {
      cards = [ { suit = Heart; value = Four }; { suit = Heart; value = Nine } ];
      chips = 980;
      bet = 20;
      active = true;
    };
    {
      cards = [ { suit = Diamond; value = A }; { suit = Club; value = A } ];
      chips = 1000;
      bet = 0;
      active = true;
    };
  ]

let bet_player_list =
  [
    {
      cards = [ { suit = Spade; value = Two }; { suit = Heart; value = Ten } ];
      chips = 990;
      bet = 10;
      active = true;
    };
    {
      cards = [ { suit = Heart; value = Four }; { suit = Heart; value = Nine } ];
      chips = 980;
      bet = 20;
      active = true;
    };
    {
      cards = [ { suit = Diamond; value = A }; { suit = Club; value = A } ];
      chips = 900;
      bet = 100;
      active = true;
    };
  ]

let call_player_list =
  [
    {
      cards = [ { suit = Spade; value = Two }; { suit = Heart; value = Ten } ];
      chips = 990;
      bet = 10;
      active = true;
    };
    {
      cards = [ { suit = Heart; value = Four }; { suit = Heart; value = Nine } ];
      chips = 980;
      bet = 20;
      active = true;
    };
    {
      cards = [ { suit = Diamond; value = A }; { suit = Club; value = A } ];
      chips = 980;
      bet = 20;
      active = true;
    };
  ]

let flop_player_list =
  [
    {
      cards = [ { suit = Spade; value = Two }; { suit = Heart; value = Ten } ];
      chips = 990;
      bet = 0;
      active = true;
    };
    {
      cards = [ { suit = Heart; value = Four }; { suit = Heart; value = Nine } ];
      chips = 980;
      bet = 0;
      active = true;
    };
    {
      cards = [ { suit = Diamond; value = A }; { suit = Club; value = A } ];
      chips = 1000;
      bet = 0;
      active = true;
    };
  ]

let blinds_less_state =
  {
    players = blind_less_players;
    deck = phys_deck;
    board = PreFlop;
    pot = 30;
    raised = 20;
    bblind = 1;
    sblind = 0;
    last_raised = 2;
    round = 0;
  }

let flop_state =
  {
    players = phys_player_list;
    deck = phys_deck;
    board = PreFlop;
    pot = 50;
    raised = 0;
    bblind = 3;
    sblind = 2;
    last_raised = 2;
    round = 1;
  }

let post_flop_state =
  {
    players = flop_player_list;
    deck = discard phys_deck 3;
    board =
      Flop
        [
          { suit = Spade; value = Two };
          { suit = Spade; value = Three };
          { suit = Spade; value = Four };
        ];
    pot = 50;
    raised = 0;
    bblind = 3;
    sblind = 2;
    last_raised = 2;
    round = 1;
  }

let post_turn_state =
  {
    players = flop_player_list;
    deck = discard phys_deck 4;
    board =
      Turn
        [
          { suit = Spade; value = Five };
          { suit = Spade; value = Two };
          { suit = Spade; value = Three };
          { suit = Spade; value = Four };
        ];
    pot = 50;
    raised = 0;
    bblind = 3;
    sblind = 2;
    last_raised = 2;
    round = 1;
  }

let phy_state =
  {
    players = phys_player_list;
    deck = phys_deck;
    board = PreFlop;
    pot = 30;
    raised = 20;
    bblind = 1;
    sblind = 0;
    last_raised = 2;
    round = 0;
  }

let raise_state =
  {
    players = bet_player_list;
    deck = phys_deck;
    board = PreFlop;
    pot = 130;
    raised = 100;
    bblind = 1;
    sblind = 0;
    last_raised = 2;
    round = 0;
  }

let call_state =
  {
    players = call_player_list;
    deck = phys_deck;
    board = PreFlop;
    pot = 50;
    raised = 20;
    bblind = 1;
    sblind = 0;
    last_raised = 2;
    round = 0;
  }

let state_tests =
  [
    player_bet_test "bet test 1" phy_state 100 3 bet_player_list;
    raise_test "raise test 1" phy_state 100 3 raise_state;
    call_test "raise test 1" phy_state 20 3 call_state;
    blind_test "blind test 1" blinds_less_state 3 phys_player_list;
    make_flop_test "make flop test" flop_state 3 post_flop_state;
    make_turn_test "make turn test" post_flop_state 3 post_turn_state;
  ]

let functions_tests =
  [
    init_deal_cards_test "2 players" 2
      [
        (* player 1 *)
        {
          cards =
            [ { suit = Spade; value = Two }; { suit = Spade; value = Three } ];
          chips = 1000;
          bet = 0;
          active = true;
        };
        (* player 2 *)
        {
          cards =
            [ { suit = Spade; value = Four }; { suit = Spade; value = Five } ];
          chips = 1000;
          bet = 0;
          active = true;
        };
      ];
    init_deal_cards_test "3 players" 3
      [
        (* player 1 *)
        {
          cards =
            [ { suit = Spade; value = Two }; { suit = Spade; value = Three } ];
          chips = 1000;
          bet = 0;
          active = true;
        };
        (* player 2 *)
        {
          cards =
            [ { suit = Spade; value = Four }; { suit = Spade; value = Five } ];
          chips = 1000;
          bet = 0;
          active = true;
        };
        (* player 3 *)
        {
          cards =
            [ { suit = Spade; value = Six }; { suit = Spade; value = Seven } ];
          chips = 1000;
          bet = 0;
          active = true;
        };
      ];
    init_deal_cards_test "4 players" 4
      [
        (* player 1 *)
        {
          cards =
            [ { suit = Spade; value = Two }; { suit = Spade; value = Three } ];
          chips = 1000;
          bet = 0;
          active = true;
        };
        (* player 2 *)
        {
          cards =
            [ { suit = Spade; value = Four }; { suit = Spade; value = Five } ];
          chips = 1000;
          bet = 0;
          active = true;
        };
        (* player 3 *)
        {
          cards =
            [ { suit = Spade; value = Six }; { suit = Spade; value = Seven } ];
          chips = 1000;
          bet = 0;
          active = true;
        };
        (* player 4 *)
        {
          cards =
            [ { suit = Spade; value = Eight }; { suit = Spade; value = Nine } ];
          chips = 1000;
          bet = 0;
          active = true;
        };
      ];
    init_deal_cards_test "5 players" 5
      [
        (* player 1 *)
        {
          cards =
            [ { suit = Spade; value = Two }; { suit = Spade; value = Three } ];
          chips = 1000;
          bet = 0;
          active = true;
        };
        (* player 2 *)
        {
          cards =
            [ { suit = Spade; value = Four }; { suit = Spade; value = Five } ];
          chips = 1000;
          bet = 0;
          active = true;
        };
        (* player 3 *)
        {
          cards =
            [ { suit = Spade; value = Six }; { suit = Spade; value = Seven } ];
          chips = 1000;
          bet = 0;
          active = true;
        };
        (* player 4 *)
        {
          cards =
            [ { suit = Spade; value = Eight }; { suit = Spade; value = Nine } ];
          chips = 1000;
          bet = 0;
          active = true;
        };
        (* player 5 *)
        {
          cards = [ { suit = Spade; value = Ten }; { suit = Spade; value = J } ];
          chips = 1000;
          bet = 0;
          active = true;
        };
      ];
    init_deal_cards_test "6 players" 6
      [
        (* player 1 *)
        {
          cards =
            [ { suit = Spade; value = Two }; { suit = Spade; value = Three } ];
          chips = 1000;
          bet = 0;
          active = true;
        };
        (* player 2 *)
        {
          cards =
            [ { suit = Spade; value = Four }; { suit = Spade; value = Five } ];
          chips = 1000;
          bet = 0;
          active = true;
        };
        (* player 3 *)
        {
          cards =
            [ { suit = Spade; value = Six }; { suit = Spade; value = Seven } ];
          chips = 1000;
          bet = 0;
          active = true;
        };
        (* player 4 *)
        {
          cards =
            [ { suit = Spade; value = Eight }; { suit = Spade; value = Nine } ];
          chips = 1000;
          bet = 0;
          active = true;
        };
        (* player 5 *)
        {
          cards = [ { suit = Spade; value = Ten }; { suit = Spade; value = J } ];
          chips = 1000;
          bet = 0;
          active = true;
        };
        (* player 6 *)
        {
          cards = [ { suit = Spade; value = Q }; { suit = Spade; value = K } ];
          chips = 1000;
          bet = 0;
          active = true;
        };
      ];
    init_deal_cards_test "7 players" 7
      [
        (* player 1 *)
        {
          cards =
            [ { suit = Spade; value = Two }; { suit = Spade; value = Three } ];
          chips = 1000;
          bet = 0;
          active = true;
        };
        (* player 2 *)
        {
          cards =
            [ { suit = Spade; value = Four }; { suit = Spade; value = Five } ];
          chips = 1000;
          bet = 0;
          active = true;
        };
        (* player 3 *)
        {
          cards =
            [ { suit = Spade; value = Six }; { suit = Spade; value = Seven } ];
          chips = 1000;
          bet = 0;
          active = true;
        };
        (* player 4 *)
        {
          cards =
            [ { suit = Spade; value = Eight }; { suit = Spade; value = Nine } ];
          chips = 1000;
          bet = 0;
          active = true;
        };
        (* player 5 *)
        {
          cards = [ { suit = Spade; value = Ten }; { suit = Spade; value = J } ];
          chips = 1000;
          bet = 0;
          active = true;
        };
        (* player 6 *)
        {
          cards = [ { suit = Spade; value = Q }; { suit = Spade; value = K } ];
          chips = 1000;
          bet = 0;
          active = true;
        };
        (* player 7 *)
        {
          cards = [ { suit = Spade; value = A }; { suit = Club; value = Two } ];
          chips = 1000;
          bet = 0;
          active = true;
        };
      ];
    init_deal_cards_test "8 players" 8
      [
        (* player 1 *)
        {
          cards =
            [ { suit = Spade; value = Two }; { suit = Spade; value = Three } ];
          chips = 1000;
          bet = 0;
          active = true;
        };
        (* player 2 *)
        {
          cards =
            [ { suit = Spade; value = Four }; { suit = Spade; value = Five } ];
          chips = 1000;
          bet = 0;
          active = true;
        };
        (* player 3 *)
        {
          cards =
            [ { suit = Spade; value = Six }; { suit = Spade; value = Seven } ];
          chips = 1000;
          bet = 0;
          active = true;
        };
        (* player 4 *)
        {
          cards =
            [ { suit = Spade; value = Eight }; { suit = Spade; value = Nine } ];
          chips = 1000;
          bet = 0;
          active = true;
        };
        (* player 5 *)
        {
          cards = [ { suit = Spade; value = Ten }; { suit = Spade; value = J } ];
          chips = 1000;
          bet = 0;
          active = true;
        };
        (* player 6 *)
        {
          cards = [ { suit = Spade; value = Q }; { suit = Spade; value = K } ];
          chips = 1000;
          bet = 0;
          active = true;
        };
        (* player 7 *)
        {
          cards = [ { suit = Spade; value = A }; { suit = Club; value = Two } ];
          chips = 1000;
          bet = 0;
          active = true;
        };
        (* player 8 *)
        {
          cards =
            [ { suit = Club; value = Three }; { suit = Club; value = Four } ];
          chips = 1000;
          bet = 0;
          active = true;
        };
      ];
    init_deal_cards_test "9 players" 9
      [
        (* player 1 *)
        {
          cards =
            [ { suit = Spade; value = Two }; { suit = Spade; value = Three } ];
          chips = 1000;
          bet = 0;
          active = true;
        };
        (* player 2 *)
        {
          cards =
            [ { suit = Spade; value = Four }; { suit = Spade; value = Five } ];
          chips = 1000;
          bet = 0;
          active = true;
        };
        (* player 3 *)
        {
          cards =
            [ { suit = Spade; value = Six }; { suit = Spade; value = Seven } ];
          chips = 1000;
          bet = 0;
          active = true;
        };
        (* player 4 *)
        {
          cards =
            [ { suit = Spade; value = Eight }; { suit = Spade; value = Nine } ];
          chips = 1000;
          bet = 0;
          active = true;
        };
        (* player 5 *)
        {
          cards = [ { suit = Spade; value = Ten }; { suit = Spade; value = J } ];
          chips = 1000;
          bet = 0;
          active = true;
        };
        (* player 6 *)
        {
          cards = [ { suit = Spade; value = Q }; { suit = Spade; value = K } ];
          chips = 1000;
          bet = 0;
          active = true;
        };
        (* player 7 *)
        {
          cards = [ { suit = Spade; value = A }; { suit = Club; value = Two } ];
          chips = 1000;
          bet = 0;
          active = true;
        };
        (* player 8 *)
        {
          cards =
            [ { suit = Club; value = Three }; { suit = Club; value = Four } ];
          chips = 1000;
          bet = 0;
          active = true;
        };
        (* player 9 *)
        {
          cards =
            [ { suit = Club; value = Five }; { suit = Club; value = Six } ];
          chips = 1000;
          bet = 0;
          active = true;
        };
      ];
    init_deal_cards_test "10 players" 10
      [
        (* player 1 *)
        {
          cards =
            [ { suit = Spade; value = Two }; { suit = Spade; value = Three } ];
          chips = 1000;
          bet = 0;
          active = true;
        };
        (* player 2 *)
        {
          cards =
            [ { suit = Spade; value = Four }; { suit = Spade; value = Five } ];
          chips = 1000;
          bet = 0;
          active = true;
        };
        (* player 3 *)
        {
          cards =
            [ { suit = Spade; value = Six }; { suit = Spade; value = Seven } ];
          chips = 1000;
          bet = 0;
          active = true;
        };
        (* player 4 *)
        {
          cards =
            [ { suit = Spade; value = Eight }; { suit = Spade; value = Nine } ];
          chips = 1000;
          bet = 0;
          active = true;
        };
        (* player 5 *)
        {
          cards = [ { suit = Spade; value = Ten }; { suit = Spade; value = J } ];
          chips = 1000;
          bet = 0;
          active = true;
        };
        (* player 6 *)
        {
          cards = [ { suit = Spade; value = Q }; { suit = Spade; value = K } ];
          chips = 1000;
          bet = 0;
          active = true;
        };
        (* player 7 *)
        {
          cards = [ { suit = Spade; value = A }; { suit = Club; value = Two } ];
          chips = 1000;
          bet = 0;
          active = true;
        };
        (* player 8 *)
        {
          cards =
            [ { suit = Club; value = Three }; { suit = Club; value = Four } ];
          chips = 1000;
          bet = 0;
          active = true;
        };
        (* player 9 *)
        {
          cards =
            [ { suit = Club; value = Five }; { suit = Club; value = Six } ];
          chips = 1000;
          bet = 0;
          active = true;
        };
        (* player 10 *)
        {
          cards =
            [ { suit = Club; value = Seven }; { suit = Club; value = Eight } ];
          chips = 1000;
          bet = 0;
          active = true;
        };
      ];
  ]

let suite =
  "test suite for project"
  >::: List.flatten [ card_tests; functions_tests; state_tests; deck_tests ]

let _ = run_test_tt_main suite
