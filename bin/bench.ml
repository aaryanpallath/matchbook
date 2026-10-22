(* Throughput and per-command latency (monotonic clock, nanosecond resolution) on a synthetic order stream:
     dune exec --release bin/bench.exe -- [commands]
   The stream follows a random-walk mid price: 55% limit orders within 10 ticks of the mid, 35% cancels of earlier
   orders, 10% market orders. The naive reference model runs on a prefix of the same stream for comparison. *)

open Matchbook
open Book

let stream n =
  let rng = Random.State.make [| 42 |] in
  let mid = ref 10_000 in
  Array.init n (fun i ->
      let id = i + 1 in
      if Random.State.int rng 100 < 2 then mid := !mid + Random.State.int rng 3 - 1;
      let side = if Random.State.bool rng then Buy else Sell in
      let r = Random.State.int rng 100 in
      if r < 55 then
        let offset = Random.State.int rng 11 in
        (* buys mostly below the mid and sells above it, so a few cross and most rest *)
        let price = if side = Buy then !mid - offset + 2 else !mid + offset - 2 in
        Limit { id; side; price; qty = 1 + Random.State.int rng 100 }
      else if r < 90 then Cancel { id = 1 + Random.State.int rng (max 1 i) }
      else Market { id; side; qty = 1 + Random.State.int rng 200 })

let time submit cmds =
  let lat = Array.make (Array.length cmds) 0.0 in
  let t0 = Mtime_clock.counter () in
  Array.iteri
    (fun i c ->
      let s = Mtime_clock.counter () in
      ignore (Sys.opaque_identity (submit c));
      lat.(i) <- Mtime.Span.to_float_ns (Mtime_clock.count s) /. 1e9)
    cmds;
  let total = Mtime.Span.to_float_ns (Mtime_clock.count t0) /. 1e9 in
  Array.sort compare lat;
  let pct p = 1e6 *. lat.(min (Array.length lat - 1) (int_of_float (p *. float (Array.length lat)))) in
  (total, pct 0.5, pct 0.99, pct 0.999)

let () =
  let n = if Array.length Sys.argv > 1 then int_of_string Sys.argv.(1) else 1_000_000 in
  let cmds = stream n in
  let b = create () in
  ignore (time (submit (create ())) (Array.sub cmds 0 (min n 100_000)));  (* warm up *)
  let total, p50, p99, p999 = time (submit b) cmds in
  Printf.printf "book       %9d commands  %6.2f s  %10.0f commands/s  p50 %.2f us  p99 %.2f us  p99.9 %.2f us\n" n total
    (float n /. total) p50 p99 p999;
  Printf.printf "           %d orders resting at the end, %s\n" (List.length (resting b))
    (match check b with Ok () -> "invariants hold" | Error e -> "BROKEN: " ^ e);
  let m = min n 20_000 in
  let r = Reference.create () in
  let total, p50, p99, _ = time (Reference.submit r) (Array.sub cmds 0 m) in
  Printf.printf "reference  %9d commands  %6.2f s  %10.0f commands/s  p50 %.2f us  p99 %.2f us\n" m total
    (float m /. total) p50 p99
