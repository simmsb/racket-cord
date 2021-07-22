# racket-cord

A low-level library for interfacing with Discord using Racket.

[![Build Status](https://github.com/nitros12/racket-cord/actions/workflows/ci.yml/badge.svg)](https://github.com/nitros12/racket-cord/actions/workflows/ci.yml)

[Racket package](https://pkgd.racket-lang.org/pkgn/package/racket-cord)

[Docs](https://docs.racket-lang.org/racket-cord/index.html)

# Design Notes
The library is focused on only a couple things and doing those things properly:
1. Providing the network plumbing for receiving and sending gateway events
2. Providing convenience bindings for the HTTP endpoints

Thus, this library is quite low level. In general, data is exposed directly as returned by the Discord API,
without extra conversions into other types. There is nearly no caching of state in the client.

The rationale can be found in this [commit message](https://github.com/simmsb/racket-cord/commit/64b8f1de97fccb01487571362e2b4bac749c3691)

Essentially, I don't have the time nor energy to maintain typed wrappers when Discord's API
is so unstable, the documentation so bad, and no machine-readable specs exist.

Typed wrappers or cached client-side state should not be difficult to implement on top of
this library via the gateway events, if someone is interested in subjecting themselves to that pain.
If you make one, let us know and we will feature it here. :)

# Sample Projects

* R16: A Racket trick bot for Discord - https://sr.ht/~williewillus/r16
