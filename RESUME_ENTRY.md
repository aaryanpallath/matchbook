# matchbook — Limit Order Book Matching Engine in OCaml
Tech: OCaml, dune, QCheck, ppx_expect, GitHub Actions
- Built a price-time priority matching engine in OCaml supporting limit, market and cancel orders, with O(log n) cancels via per-level sequence maps and an id index.
- Verified it with differential property testing against a naive reference model on 4,000 random order streams, checking events, resting orders and book invariants after every command, plus expect tests.
- Processes 2.4M commands/s at 0.25 µs median and under 1 µs p99 latency, 33x faster than the reference model.
