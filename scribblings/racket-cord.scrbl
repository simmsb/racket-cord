#lang scribble/manual
@require[@for-label[racket-cord]
                   @for-label[@except-in[racket/base member]]]

@title{racket-cord: Racket discord library}
@author{@author+email["Ben Simms" "ben@bensimms.moe"]}

@defmodule[racket-cord]

@section{Example}

Example usage of the library:

@racketblock[
(require racket-cord)

(define bot-token (getenv "BOT_TOKEN"))

(define myclient (make-client bot-token #:auto-shard #t))

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

(start-client myclient)]

@section{Client}

@defstruct[
  client
  ([shards list?]
   [user user?]
   [guilds hash?]
   [private-channels hash?]
   [events hash?]
   [http-client http:http-client?]
   [token string?]
   [running semaphore?])
   #:mutable
   #:transparent]{
Stores the state of the client.
}

@defproc[(make-client [token string?]
                      [#:token-type token-type symbol? 'bot]
                      [#:auto-shard auto-shard boolean? #f]
                      [#:shard-count shard-count integer? 1]) client?]{
Makes a discord client with the passed token.

@racket[token]: The token to use.

@racket[#:token-type]: The type of token, either @racket['bot], @racket['bearer] or @racket['client].

@racket[#:auto-shard]: Whether to request the number of shards from discord, only usable for bot clients.

@racket[#:shard-count]: If auto-shard is not passed, the number of shards to connect will be such.}
