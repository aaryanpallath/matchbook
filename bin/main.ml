(* An interactive book. Commands, one per line:
     buy 10 @ 101     a limit order: quantity @ price
     sell 5           a market order
     cancel 3         cancel order #3
     book             show the top 5 levels each side
   Order ids are assigned in sequence and printed with each event. *)

open Matchbook
open Book

let show_book b =
  let row side = List.map (fun (p, q) -> Printf.sprintf "%5d @ %d" q p) (depth b side 5) in
  print_endline "  asks:";
  List.iter (fun l -> print_endline ("    " ^ l)) (List.rev (row Sell));
  print_endline "  bids:";
  List.iter (fun l -> print_endline ("    " ^ l)) (row Buy)

let parse next_id line =
  let side = function "buy" -> Some Buy | "sell" -> Some Sell | _ -> None in
  match String.split_on_char ' ' (String.trim line) |> List.filter (( <> ) "") with
  | [ s; q; "@"; p ] -> (
      match (side s, int_of_string_opt q, int_of_string_opt p) with
      | Some side, Some qty, Some price -> Ok (Limit { id = next_id; side; price; qty })
      | _ -> Error "expected: buy|sell QTY @ PRICE")
  | [ s; q ] when side s <> None -> (
      match (side s, int_of_string_opt q) with
      | Some side, Some qty -> Ok (Market { id = next_id; side; qty })
      | _ -> Error "expected: buy|sell QTY")
  | [ "cancel"; id ] -> (
      match int_of_string_opt id with Some id -> Ok (Cancel { id }) | None -> Error "expected: cancel ID")
  | _ -> Error "commands: buy|sell QTY [@ PRICE], cancel ID, book"

let () =
  let b = create () in
  let next_id = ref 1 in
  try
    while true do
      let line = input_line stdin in
      if String.trim line = "book" then show_book b
      else if String.trim line <> "" then
        match parse !next_id line with
        | Error e -> print_endline e
        | Ok cmd ->
            (match cmd with Cancel _ -> () | _ -> incr next_id);
            List.iter (fun e -> print_endline (event_to_string e)) (submit b cmd)
    done
  with End_of_file -> ()
