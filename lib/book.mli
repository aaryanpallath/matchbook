(** A limit order book for one instrument, with price-time priority matching.

    An incoming order trades against the best opposite price first, and within a price level against the order
    that arrived first. Trades happen at the resting (maker) order's price. Whatever is left of a limit order rests
    in the book; whatever is left of a market order is reported unfilled and discarded. Prices are integer ticks,
    so there is no floating-point rounding anywhere. *)

type side = Buy | Sell

type command =
  | Limit of { id : int; side : side; price : int; qty : int }
  | Market of { id : int; side : side; qty : int }
  | Cancel of { id : int }

type event =
  | Fill of { taker : int; maker : int; price : int; qty : int }
  | Rested of { id : int; side : side; price : int; qty : int }
  | Cancelled of { id : int; qty : int }
  | Unfilled of { id : int; qty : int }  (** the part of a market order with nothing left to trade against *)
  | Rejected of { id : int; reason : string }

type order = { id : int; side : side; price : int; qty : int }

type t

val create : unit -> t

val submit : t -> command -> event list
(** Process one command and return what happened, in order. Rejected: a non-positive price or quantity, an order
    id used before (ids are never reused, even after an order is filled or cancelled), and a cancel for an order
    that is not resting. *)

val best_bid : t -> (int * int) option
(** Best bid price and the total quantity resting at it. *)

val best_ask : t -> (int * int) option

val depth : t -> side -> int -> (int * int) list
(** The best [n] price levels on one side, best first, as (price, total quantity). *)

val resting : t -> order list
(** Every resting order, bids then asks, each side in priority order. *)

val check : t -> (unit, string) result
(** Verify the book's invariants: not crossed (best bid < best ask), no empty levels or orders with zero quantity,
    level totals equal to the sum of their orders, and the id index matching the levels exactly. *)

val side_to_string : side -> string
val event_to_string : event -> string
