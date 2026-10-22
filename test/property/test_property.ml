(* Property tests: on random streams of orders, the book must agree exactly with the naive reference model, keep
   its invariants after every command, and conserve quantity. *)

open Matchbook
open Book

(* Prices cluster around 100 so orders cross often; some cancels target ids that never rested, were filled or were
   already cancelled; a few orders reuse an id or have a zero quantity, to exercise the rejections. *)
let gen_commands =
  let open QCheck.Gen in
  let side = oneof_list [ Buy; Sell ] in
  let* n = int_range 1 300 in
  let rec go i acc =
    if i > n then return (List.rev acc)
    else
      let* kind = int_range 0 99 in
      let* cmd =
        if kind < 60 then
          map3 (fun s p q -> Limit { id = i; side = s; price = p; qty = q }) side (int_range 95 105) (int_range 1 20)
        else if kind < 85 then map (fun target -> Cancel { id = target }) (int_range 1 (i + 2))
        else if kind < 95 then map2 (fun s q -> Market { id = i; side = s; qty = q }) side (int_range 1 40)
        else
          map2 (fun reuse q -> Limit { id = reuse; side = Buy; price = 100; qty = q }) (int_range 1 i) (int_range 0 3)
      in
      go (i + 1) (cmd :: acc)
  in
  go 1 []

let show = function
  | Limit { id; side; price; qty } -> Printf.sprintf "L%d %s %d@%d" id (side_to_string side) qty price
  | Market { id; side; qty } -> Printf.sprintf "M%d %s %d" id (side_to_string side) qty
  | Cancel { id } -> Printf.sprintf "C%d" id

let arb = QCheck.make gen_commands ~print:(fun cs -> String.concat "; " (List.map show cs))

let agrees_with_reference =
  QCheck.Test.make ~name:"same events and resting orders as the reference model" ~count:2000 arb (fun cmds ->
      let book = create () and model = Reference.create () in
      List.for_all
        (fun c ->
          let got = submit book c and want = Reference.submit model c in
          if got <> want then
            QCheck.Test.fail_reportf "after %s:\nbook:      %s\nreference: %s" (show c)
              (String.concat ", " (List.map event_to_string got))
              (String.concat ", " (List.map event_to_string want));
          (match check book with Ok () -> () | Error e -> QCheck.Test.fail_reportf "invariant broken: %s" e);
          resting book = Reference.resting model)
        cmds)

(* Every accepted order's quantity ends up filled, resting, cancelled or unfilled: none is created or lost. *)
let conserves_quantity =
  QCheck.Test.make ~name:"quantity is conserved" ~count:2000 arb (fun cmds ->
      let book = create () in
      let submitted = ref 0 and accounted = ref 0 in
      List.iter
        (fun c ->
          let events = submit book c in
          let accepted = not (List.exists (function Rejected _ -> true | _ -> false) events) in
          (match c with
          | (Limit { qty; _ } | Market { qty; _ }) when accepted -> submitted := !submitted + qty
          | _ -> ());
          List.iter
            (function
              | Fill { qty; _ } -> accounted := !accounted + (2 * qty) (* the taker's and the maker's share *)
              | Cancelled { qty; _ } | Unfilled { qty; _ } -> accounted := !accounted + qty
              | Rested _ | Rejected _ -> ())
            events)
        cmds;
      let resting = List.fold_left (fun s (o : order) -> s + o.qty) 0 (resting book) in
      !accounted + resting = !submitted)

let () = exit (QCheck_base_runner.run_tests ~verbose:true [ agrees_with_reference; conserves_quantity ])
