# racket-cord

A library for interfacing with Discord using Racket.

[![Build Status](https://github.com/nitros12/racket-cord/actions/workflows/ci.yml/badge.svg)](https://github.com/nitros12/racket-cord/actions/workflows/ci.yml)

[Racket package](https://pkgd.racket-lang.org/pkgn/package/racket-cord)

[Docs](https://docs.racket-lang.org/racket-cord/index.html)

# Design Notes
## Typing
* Effort is ongoing to provide (contracted) structs for all Discord API objects in `data.rkt`.
* Since this is an ongoing effort, some data may remain untyped/in hash-map form.
If something has no contract or has an `any/c` contract, this is probably the case.
* Please check `data.rkt` for more details.

## Representation of Null Values

* In bindings of Discord data types, this library does not distinguish between entries
omitted in responses from Discord, or entries with a `null` value.
Both are converted to `#f`, as is standard Racket convention.
* Note that this causes ambiguity in the case of nullable booleans (which are a terrible idea in the first place).
* Note that cleanups and refactors are still in progress and this convention is not observed everywhere yet.

# Sample Projects

* R16: A Racket trick bot for Discord - [https://sr.ht/~williewillus/r16]()
