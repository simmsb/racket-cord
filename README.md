# racket-cord

A library for interfacing with Discord using racket.

[![Build Status](https://github.com/nitros12/racket-cord/actions/workflows/ci.yml/badge.svg)](https://github.com/nitros12/racket-cord/actions/workflows/ci.yml)

[Racket package](https://pkgd.racket-lang.org/pkgn/package/racket-cord)

[Docs](https://docs.racket-lang.org/racket-cord/index.html)

# Example

```racket
#lang racket

(require racket-cord)

(define bot-token (getenv "BOT_TOKEN"))

(define myclient (make-client bot-token #:intents '(intent-guilds intent-guild-messages)
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
