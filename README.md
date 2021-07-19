# racket-cord

A library for interfacing with Discord using Racket.

[![Build Status](https://github.com/nitros12/racket-cord/actions/workflows/ci.yml/badge.svg)](https://github.com/nitros12/racket-cord/actions/workflows/ci.yml)

[Racket package](https://pkgd.racket-lang.org/pkgn/package/racket-cord)

[Docs](https://docs.racket-lang.org/racket-cord/index.html)

# Design Notes
This library is quite low level. In general, data is returned directly as given by the Discord API,
without extra conversions into other types.

The rationale can be found in this [commit message](https://github.com/simmsb/racket-cord/commit/64b8f1de97fccb01487571362e2b4bac749c3691)

Essentially, I don't have the time nor energy to maintain typed wrappers when Discord's API
is so unstable, the documentation so bad, and no machine-readable specs exist.

It shouldn't be too hard to provide a higher-level library on top of this one that exposes typed
wrappers, if someone is interested in subjecting themselves to that pain.

# Sample Projects

* R16: A Racket trick bot for Discord - https://sr.ht/~williewillus/r16
