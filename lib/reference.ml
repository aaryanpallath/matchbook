(* A deliberately naive order book: one flat list of resting orders, filtered and sorted again for every incoming
   order. It is far too slow for real use, but short enough to check by reading, so it is the oracle the real book
   is tested against: on any stream of commands, both must produce the same events and the same resting orders. *)

open Book

type t = { mutable resting : (int * order) list; (* arrival sequence number, order *) used : (int, unit) Hashtbl.t;
           mutable seq : int }

let create () = { resting = []; used = Hashtbl.create 64; seq = 0 }

(* better first: the highest bid or the lowest ask, then the earlier arrival *)
let priority side (s1, (o1 : order)) (s2, (o2 : order)) =
  let by_price = match side with Buy -> compare o2.price o1.price | Sell -> compare o1.price o2.price in
  if by_price <> 0 then by_price else compare s1 s2

let opposite = function Buy -> Sell | Sell -> Buy

let trade t ~id ~side ~limit ~qty =
  let acceptable (o : order) = match side with Buy -> o.price <= limit | Sell -> o.price >= limit in
  let makers =
    List.filter (fun (_, (o : order)) -> o.side <> side && acceptable o) t.resting
    |> List.sort (priority (opposite side))
  in
  let rec go qty fills = function
    | (s, (o : order)) :: rest when qty > 0 ->
        let q = min qty o.qty in
        t.resting <-
          List.filter_map
            (fun (s', (o' : order)) ->
              if s' <> s then Some (s', o') else if o'.qty = q then None else Some (s', { o' with qty = o'.qty - q }))
            t.resting;
        go (qty - q) (Fill { taker = id; maker = o.id; price = o.price; qty = q } :: fills) rest
    | _ -> (qty, List.rev fills)
  in
  go qty [] makers

let reject t ~id ~qty ~price =
  if qty <= 0 then Some "quantity must be positive"
  else if price <= 0 then Some "price must be positive"
  else if Hashtbl.mem t.used id then Some "duplicate order id"
  else None

let submit t = function
  | Limit { id; side; price; qty } -> (
      match reject t ~id ~qty ~price with
      | Some reason -> [ Rejected { id; reason } ]
      | None ->
          Hashtbl.replace t.used id ();
          let left, fills = trade t ~id ~side ~limit:price ~qty in
          if left = 0 then fills
          else (
            t.seq <- t.seq + 1;
            t.resting <- t.resting @ [ (t.seq, { id; side; price; qty = left }) ];
            fills @ [ Rested { id; side; price; qty = left } ]))
  | Market { id; side; qty } -> (
      match reject t ~id ~qty ~price:1 with
      | Some reason -> [ Rejected { id; reason } ]
      | None ->
          Hashtbl.replace t.used id ();
          let limit = match side with Buy -> max_int | Sell -> min_int in
          let left, fills = trade t ~id ~side ~limit ~qty in
          fills @ if left = 0 then [] else [ Unfilled { id; qty = left } ])
  | Cancel { id } -> (
      match List.find_opt (fun (_, (o : order)) -> o.id = id) t.resting with
      | None -> [ Rejected { id; reason = "not a resting order" } ]
      | Some (_, o) ->
          t.resting <- List.filter (fun (_, (o' : order)) -> o'.id <> id) t.resting;
          [ Cancelled { id; qty = o.qty } ])

let resting t =
  let side s = List.filter (fun (_, (o : order)) -> o.side = s) t.resting |> List.sort (priority s) |> List.map snd in
  side Buy @ side Sell
