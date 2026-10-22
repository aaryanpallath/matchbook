open Matchbook
open Book

let run book cmds =
  List.iter (fun c -> List.iter (fun e -> print_endline (event_to_string e)) (submit book c)) cmds;
  match check book with Ok () -> () | Error e -> print_endline ("BROKEN: " ^ e)

let show_depth book =
  let line side = List.map (fun (p, q) -> Printf.sprintf "%d@%d" q p) (depth book side 5) |> String.concat " " in
  Printf.printf "bids: %s | asks: %s\n" (line Buy) (line Sell)

let%expect_test "an order that crosses trades at the resting price, best price first, then rests the rest" =
  let b = create () in
  run b
    [ Limit { id = 1; side = Sell; price = 101; qty = 5 };
      Limit { id = 2; side = Sell; price = 100; qty = 3 };
      Limit { id = 3; side = Buy; price = 102; qty = 10 } ];
  show_depth b;
  [%expect
    {|
    rest    #1 sell 5 @ 101
    rest    #2 sell 3 @ 100
    fill    #3 x #2  3 @ 100
    fill    #3 x #1  5 @ 101
    rest    #3 buy 2 @ 102
    bids: 2@102 | asks:
    |}]

let%expect_test "within a price level, the earlier order fills first" =
  let b = create () in
  run b
    [ Limit { id = 1; side = Buy; price = 99; qty = 4 };
      Limit { id = 2; side = Buy; price = 99; qty = 4 };
      Market { id = 3; side = Sell; qty = 6 } ];
  show_depth b;
  [%expect
    {|
    rest    #1 buy 4 @ 99
    rest    #2 buy 4 @ 99
    fill    #3 x #1  4 @ 99
    fill    #3 x #2  2 @ 99
    bids: 2@99 | asks:
    |}]

let%expect_test "a market order takes what liquidity there is and reports the rest unfilled" =
  let b = create () in
  run b [ Limit { id = 1; side = Sell; price = 100; qty = 2 }; Market { id = 2; side = Buy; qty = 5 } ];
  show_depth b;
  [%expect
    {|
    rest    #1 sell 2 @ 100
    fill    #2 x #1  2 @ 100
    unfilled #2 3 (no more liquidity)
    bids:  | asks:
    |}]

let%expect_test "cancels, and the things that are rejected" =
  let b = create () in
  run b
    [ Limit { id = 1; side = Buy; price = 99; qty = 4 };
      Limit { id = 2; side = Buy; price = 99; qty = 6 };
      Cancel { id = 1 };
      Cancel { id = 1 };
      Limit { id = 1; side = Buy; price = 98; qty = 1 };
      Limit { id = 3; side = Buy; price = 98; qty = 0 };
      Limit { id = 4; side = Sell; price = 0; qty = 1 };
      Cancel { id = 77 } ];
  show_depth b;
  [%expect
    {|
    rest    #1 buy 4 @ 99
    rest    #2 buy 6 @ 99
    cancel  #1 (4 unfilled)
    reject  #1: not a resting order
    reject  #1: duplicate order id
    reject  #3: quantity must be positive
    reject  #4: price must be positive
    reject  #77: not a resting order
    bids: 6@99 | asks:
    |}]

let%expect_test "an order that does not cross rests, and the book is never left crossed" =
  let b = create () in
  run b [ Limit { id = 1; side = Buy; price = 100; qty = 1 }; Limit { id = 2; side = Sell; price = 101; qty = 1 } ];
  Printf.printf "best bid %s, best ask %s\n"
    (match best_bid b with Some (p, q) -> Printf.sprintf "%d@%d" q p | None -> "-")
    (match best_ask b with Some (p, q) -> Printf.sprintf "%d@%d" q p | None -> "-");
  [%expect {|
    rest    #1 buy 1 @ 100
    rest    #2 sell 1 @ 101
    best bid 1@100, best ask 1@101
    |}]
