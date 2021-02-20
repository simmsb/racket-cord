# racket-cord

A library for interfacing with Discord using racket.

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

# Example

```racket
#lang racket

(require racket-cord)

(define bot-token (getenv "BOT_TOKEN"))

(define myclient (make-client bot-token #:intents (list intent-guilds intent-guild-messages)
                                        #:auto-shard #t))

(on-event
 'message-create myclient
 (lambda (client message)
   (unless (string=? (user-id (message-author message)) ; dont reply to ourselves
                     (user-id (client-user client)))
     (cond
       [(string-prefix? (message-content message) "!echo ")
        (http:create-message client (message-channel-id message)
                             (string-trim (message-content message) "!echo " #:right? #f))]))))

;; logging stufff

(define dr (make-log-receiver discord-logger 'debug))

(thread
 (thunk
  (let loop ()
    (let ([v (sync dr)])
      (printf "[~a] ~a\n" (vector-ref v 0)
              (vector-ref v 1)))
    (loop))))

(start-client myclient)
```

# rationale
I wanted to write a discord lib in a lisp,
there were already some for CLisp at the time of writing so I chose to
write one in a scheme, so I chose racket.

Maybe now I would have tried to make it more general, idk.
