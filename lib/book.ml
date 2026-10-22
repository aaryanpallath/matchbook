type side = Buy | Sell

type command =
  | Limit of { id : int; side : side; price : int; qty : int }
  | Market of { id : int; side : side; qty : int }
  | Cancel of { id : int }

type event =
  | Fill of { taker : int; maker : int; price : int; qty : int }
  | Rested of { id : int; side : side; price : int; qty : int }
  | Cancelled of { id : int; qty : int }
  | Unfilled of { id : int; qty : int }
  | Rejected of { id : int; reason : string }

type order = { id : int; side : side; price : int; qty : int }

module IntMap = Map.Make (Int)

(* A price level keeps its orders keyed by arrival sequence number: the smallest key is first in line, and a cancel
   removes an order from the middle of the queue in O(log n). *)
type level = { orders : order IntMap.t; total : int }

type t = {
  mutable bids : level IntMap.t;
  mutable asks : level IntMap.t;
  index : (int, side * int * int) Hashtbl.t;  (* resting order id -> side, price, sequence number *)
  used : (int, unit) Hashtbl.t;  (* every id ever accepted *)
  mutable seq : int;
}

let create () =
  { bids = IntMap.empty; asks = IntMap.empty; index = Hashtbl.create 1024; used = Hashtbl.create 1024; seq = 0 }

let other = function Buy -> Sell | Sell -> Buy
let levels t = function Buy -> t.bids | Sell -> t.asks
let set_levels t side m = match side with Buy -> t.bids <- m | Sell -> t.asks <- m

(* the best level on a side: the highest bid, the lowest ask *)
let best_level t side =
  match side with Buy -> IntMap.max_binding_opt t.bids | Sell -> IntMap.min_binding_opt t.asks

(* can a taker on [side] with this limit trade at [price]? *)
let crosses side ~limit price = match side with Buy -> price <= limit | Sell -> price >= limit

let rec take t ~taker ~side ~limit ~qty fills =
  if qty = 0 then (0, fills)
  else
    let maker_side = other side in
    match best_level t maker_side with
    | Some (price, level) when crosses side ~limit price ->
        let seq, maker = IntMap.min_binding level.orders in
        let q = min qty maker.qty in
        let orders =
          if q = maker.qty then (
            Hashtbl.remove t.index maker.id;
            IntMap.remove seq level.orders)
          else IntMap.add seq { maker with qty = maker.qty - q } level.orders
        in
        let book = levels t maker_side in
        set_levels t maker_side
          (if IntMap.is_empty orders then IntMap.remove price book
           else IntMap.add price { orders; total = level.total - q } book);
        take t ~taker ~side ~limit ~qty:(qty - q) (Fill { taker; maker = maker.id; price; qty = q } :: fills)
    | _ -> (qty, fills)

let rest t (o : order) =
  t.seq <- t.seq + 1;
  let book = levels t o.side in
  let level = Option.value (IntMap.find_opt o.price book) ~default:{ orders = IntMap.empty; total = 0 } in
  set_levels t o.side
    (IntMap.add o.price { orders = IntMap.add t.seq o level.orders; total = level.total + o.qty } book);
  Hashtbl.replace t.index o.id (o.side, o.price, t.seq)

let validate t ~id ~qty ~price =
  if qty <= 0 then Some "quantity must be positive"
  else if price <= 0 then Some "price must be positive"
  else if Hashtbl.mem t.used id then Some "duplicate order id"
  else None

let submit t = function
  | Limit { id; side; price; qty } -> (
      match validate t ~id ~qty ~price with
      | Some reason -> [ Rejected { id; reason } ]
      | None ->
          Hashtbl.replace t.used id ();
          let left, fills = take t ~taker:id ~side ~limit:price ~qty [] in
          let fills = List.rev fills in
          if left = 0 then fills
          else (
            rest t { id; side; price; qty = left };
            fills @ [ Rested { id; side; price; qty = left } ]))
  | Market { id; side; qty } -> (
      match validate t ~id ~qty ~price:1 with
      | Some reason -> [ Rejected { id; reason } ]
      | None ->
          Hashtbl.replace t.used id ();
          let limit = match side with Buy -> max_int | Sell -> min_int in
          let left, fills = take t ~taker:id ~side ~limit ~qty [] in
          List.rev fills @ if left = 0 then [] else [ Unfilled { id; qty = left } ])
  | Cancel { id } -> (
      match Hashtbl.find_opt t.index id with
      | None -> [ Rejected { id; reason = "not a resting order" } ]
      | Some (side, price, seq) ->
          let book = levels t side in
          let level = IntMap.find price book in
          let o = IntMap.find seq level.orders in
          let orders = IntMap.remove seq level.orders in
          set_levels t side
            (if IntMap.is_empty orders then IntMap.remove price book
             else IntMap.add price { orders; total = level.total - o.qty } book);
          Hashtbl.remove t.index id;
          [ Cancelled { id; qty = o.qty } ])

let top t side = Option.map (fun (p, l) -> (p, l.total)) (best_level t side)
let best_bid t = top t Buy
let best_ask t = top t Sell

let depth t side n =
  let all = IntMap.bindings (levels t side) in
  let all = match side with Buy -> List.rev all | Sell -> all in
  List.filteri (fun i _ -> i < n) all |> List.map (fun (p, l) -> (p, l.total))

let side_orders t side =
  let lv = IntMap.bindings (levels t side) in
  let lv = match side with Buy -> List.rev lv | Sell -> lv in
  List.concat_map (fun (_, l) -> List.map snd (IntMap.bindings l.orders)) lv

let resting t = side_orders t Buy @ side_orders t Sell

let check t =
  let ( let* ) = Result.bind in
  let fail fmt = Printf.ksprintf (fun m -> Error m) fmt in
  let counted = ref 0 in
  let check_level side price (l : level) =
    let rec go sum = function
      | [] -> if sum = l.total then Ok () else fail "level %d total is %d, orders sum to %d" price l.total sum
      | (seq, (o : order)) :: rest ->
          incr counted;
          if o.qty <= 0 then fail "order %d has quantity %d" o.id o.qty
          else if o.price <> price || o.side <> side then fail "order %d is in the wrong level" o.id
          else if Hashtbl.find_opt t.index o.id <> Some (side, price, seq) then fail "index is wrong for %d" o.id
          else go (sum + o.qty) rest
    in
    if IntMap.is_empty l.orders then fail "empty level at %d" price else go 0 (IntMap.bindings l.orders)
  in
  let check_side side =
    IntMap.fold (fun price l acc -> Result.bind acc (fun () -> check_level side price l)) (levels t side) (Ok ())
  in
  let* () =
    match (best_bid t, best_ask t) with
    | Some (b, _), Some (a, _) when b >= a -> fail "crossed book: bid %d >= ask %d" b a
    | _ -> Ok ()
  in
  let* () = check_side Buy in
  let* () = check_side Sell in
  if !counted = Hashtbl.length t.index then Ok () else fail "the index has orders that are not in the book"

let side_to_string = function Buy -> "buy" | Sell -> "sell"

let event_to_string = function
  | Fill { taker; maker; price; qty } -> Printf.sprintf "fill    #%d x #%d  %d @ %d" taker maker qty price
  | Rested { id; side; price; qty } -> Printf.sprintf "rest    #%d %s %d @ %d" id (side_to_string side) qty price
  | Cancelled { id; qty } -> Printf.sprintf "cancel  #%d (%d unfilled)" id qty
  | Unfilled { id; qty } -> Printf.sprintf "unfilled #%d %d (no more liquidity)" id qty
  | Rejected { id; reason } -> Printf.sprintf "reject  #%d: %s" id reason
