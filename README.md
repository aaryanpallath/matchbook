# matchbook: A Limit Order Book Matching Engine in OCaml

The core of an exchange: a limit order book that matches buy and sell orders by **price-time priority**. It is
written in OCaml and tested the way correctness-critical code should be. A deliberately naive reference
implementation serves as an oracle, and the real engine must agree with it exactly on thousands of random order
streams.

- **Orders:** limit, market and cancel, with integer tick prices (no floating-point rounding).
- **Matching:** an incoming order trades against the best opposite price first. Within a price level, the
  earliest order fills first. Trades happen at the resting order's price; the rest of a limit order rests, and the
  rest of a market order is reported unfilled.
- **Rejections:** non-positive prices or quantities, reused order ids, and cancels of orders that aren't resting.
- **Speed:** about **2.4 million commands per second** on a laptop, with a median of 0.25 µs per command.

## Design

```text
type t = { mutable bids : level IntMap.t;   price -> level   (best bid = max binding)
           mutable asks : level IntMap.t;   price -> level   (best ask = min binding)
           index : (id, side * price * seq) Hashtbl.t;  ... }
and level = { orders : order IntMap.t;      arrival sequence -> order (first in line = min binding)
              total : int }                 quantity at this price, kept for O(1) depth queries
```

- **Each price level keys its orders by arrival sequence number.** The order first in line is the smallest key,
  and a cancel removes an order from the middle of the queue in O(log n), found through the id index. A plain
  queue would make cancels O(n).
- **The book is persistent maps behind two mutable fields.** Matching is a recursive function that consumes the
  best level and returns the fills. It is short enough to read in one sitting (`lib/book.ml`, about 170 lines).
- **`Book.check` verifies every invariant:**
  - the book is not crossed (best bid < best ask);
  - there are no empty levels and no orders with zero quantity;
  - each level's total equals the sum of its orders;
  - the id index matches the levels exactly.

## Testing

**Differential testing against a reference model.** `lib/reference.ml` is an order book anyone can check by
reading: one flat list of resting orders, filtered and re-sorted for every incoming order. QCheck generates 2,000
random streams of up to 300 commands. Prices cluster around 100 so orders cross often; cancels target live, filled
and never-seen ids; some orders reuse an id or have zero quantity. After **every** command, the engine must:
- produce exactly the reference's events (fills in the same order, at the same prices);
- leave exactly the same resting orders in the same priority order;
- pass `Book.check`.

A second property checks **conservation**: every accepted order's quantity ends up filled, resting, cancelled or
unfilled, so none is created or lost.

**Expect tests** (`test/expect/test_book.ml`, a Jane Street testing idiom) show the behavior in readable scenarios:

```text
rest    #1 sell 5 @ 101
rest    #2 sell 3 @ 100
fill    #3 x #2  3 @ 100        best price first
fill    #3 x #1  5 @ 101
rest    #3 buy 2 @ 102          the rest of the order rests
```

**The tests catch real bugs.** As a check, I broke time priority so the newest order in a level filled first.
Both the property test (a mismatch with the reference model) and an expect test failed immediately.

```bash
opam install . --deps-only --with-test
dune test
```

## Performance

`dune exec --release bin/bench.exe`: 1,000,000 commands on a synthetic stream (a random-walk mid price; 55% limit
orders within 10 ticks of it, 35% cancels, 10% market orders), measured on an Apple-silicon MacBook with a
nanosecond monotonic clock. Three runs:

| | Commands/s | Median | p99 | p99.9 |
|---|---|---|---|---|
| matchbook | 2.36 – 2.62 million | 0.21 – 0.25 µs | 0.83 – 0.96 µs | 3.1 – 4.9 µs |
| naive reference (first 20,000 commands) | 72 thousand | 3.8 µs | 167 µs | |

The reference model is about 33 times slower even on just the first 2% of the stream. Its cost grows with every
resting order, which is why it is only a test oracle. The timing includes reading the clock twice per command, so
the true per-command cost is a little lower.

## Try it

```text
$ dune exec bin/main.exe
sell 5 @ 101
rest    #1 sell 5 @ 101
sell 3 @ 100
rest    #2 sell 3 @ 100
buy 10 @ 102
fill    #3 x #2  3 @ 100
fill    #3 x #1  5 @ 101
rest    #3 buy 2 @ 102
sell 4
fill    #4 x #3  2 @ 102
unfilled #4 2 (no more liquidity)
```

## Limitations

- One instrument, in one thread, in memory. There is no persistence, network protocol or market data feed.
- No order types beyond limit and market: no stop orders, iceberg orders, or time-in-force such as
  immediate-or-cancel. No self-trade prevention.
- The benchmark stream is synthetic; real order flow has burstier arrivals and a different cancel pattern.

Built with AI assistance (Claude Code). I reviewed the code; the tests and the broken-priority check above are
how I verified it.
