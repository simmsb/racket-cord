# racket-cord

A library for interfacing with Discord using Racket.

[![Build Status](https://github.com/nitros12/racket-cord/actions/workflows/ci.yml/badge.svg)](https://github.com/nitros12/racket-cord/actions/workflows/ci.yml)

[Racket package](https://pkgd.racket-lang.org/pkgn/package/racket-cord)

[Docs](https://docs.racket-lang.org/racket-cord/index.html)

# Design Notes
Typed wrappers will not be provided for Discord structs, and the remaining ones in the code
will be actively removed.

The rationale can be found in this [commit message](https://github.com/simmsb/racket-cord/commit/64b8f1de97fccb01487571362e2b4bac749c3691)

Essentially, I don't have the time nor energy to maintain typed wrappers when Discord's API
is so unstable, the documentation so bad, and no machine-readable specs exist.

It shouldn't be too hard to provide an addon library on top of this one that exposes typed
wrappers, if someone is interested in subjecting themselves to that pain.

# Sample Projects

* R16: A Racket trick bot for Discord - https://sr.ht/~williewillus/r16
