#lang racket

(require scribble/srcdoc
         (for-doc scribble/base scribble/manual)
         (for-label "data.rkt"))

(require json
         "constants.rkt"
         "data.rkt"
         "events.rkt"
         "gateway.rkt"
         (prefix-in http: "http.rkt")
         "utils.rkt")

(provide on-event
         (all-from-out "constants.rkt")
         (all-from-out "utils.rkt")
         (all-from-out "http.rkt")
         (struct-out game)
         (struct-out client)
         (struct-out guild)
         (struct-out guild-channel)
         (struct-out dm-channel)
         (struct-out user)
         (struct-out member)
         (struct-out message)
         (struct-out role)
         (struct-out emoji))

(define (format-token token type)
  (case type
    [(bot)    (string-append "Bot "    token)]
    [(bearer) (string-append "Bearer " token)]
    [(client) token]
    [else (raise-user-error 'format-token "~a is not a valid token type" type)]))

(provide
 (proc-doc/names
  make-client (->* (string?)
                   (#:intents (listof integer?) ; todo narrow to just known intents?
                    #:token-type (or/c 'bot 'bearer 'client)
                    #:auto-shard boolean?
                    #:shard-count integer?)
                   client?)
  ((token) ((intents null) (token-type 'bot) (auto-shard #f) (shard-count 1)))
  ("Constructs a client with the passed token."
   (linebreak)
   (linebreak)
   (racket #:auto-shard) ": If #t, ask Discord what the number of shards should be. Only applies if "
   (racket #:token-type) " is " (racket 'bot) "."
   (linebreak)
   (linebreak)
   (racket #:shard-count) ": If " (racket #:auto-shard) " is " (racket #f)
   ", the number of shards to use.")))
(define (make-client token
                     #:intents [intents null]
                     #:token-type [token-type 'bot]
                     #:auto-shard [auto-shard #f]
                     #:shard-count [shard-count 1])
  (let* ([fmt-token (format-token token token-type)]
         [client (new-client fmt-token intents)])
    (let-values ([(ws-url shards)
                  (if auto-shard
                      (http:get-ws-url-bot (client-http-client client))
                      (values (http:get-ws-url (client-http-client client))
                              shard-count))])
      (set-client-shards! client (map (lambda (n) (new-ws-client client n ws-url))
                                      (range shards))))
    client))

(define (new-client token intents)
  (let ([clnt
         (client
          null
          null
          (make-hash)
          (make-hash)
          (make-hash)
          (http:make-http-client token)
          token
          intents
          (make-semaphore 0))])
    (add-events clnt)
    clnt))

(provide
 (proc-doc/names
  start-client (-> client? void?) (client)
  ("Starts a client and begins handling events."
   (linebreak)
   "This function blocks until the client is stopped through " (racket stop-client) ".")))
(define (start-client client)
  (for ([shard (client-shards client)])
    (connect shard))
  (semaphore-wait (client-running client)))

(provide
 (proc-doc/names
  start-client-no-wait (-> client? void?) (client)
  ("Same as " (racket start-client) " but does not block the calling thread.")))
(define (start-client-no-wait client)
  (for ([shard (client-shards client)])
    (connect shard)))

(provide
 (proc-doc/names
  stop-client (-> client? void?) (client)
  ("Stops a client")))
(define (stop-client client)
  (for ([shard (client-shards client)])
    (disconnect shard))
  (semaphore-post (client-running client)))

(provide
 (proc-doc/names
  update-status (->* (client? integer?)
                     (#:since (or/c integer? #f)
                      #:activities (listof game?) ; TODO: update game to new activities spec
                      #:status (or/c "online" "dnd" "idle" "invisible" "offline") ; TODO: make it use symbols?
                      #:afk boolean?)
                     void?)
  ((client guild-id) ((since #f) (activities null) (status "online") (afk #f)))
  ("Updates the client's status.")))
(define (update-status client guild-id
                       #:since [since #f]
                       #:activities [activities null]
                       #:status [status "online"]
                       #:afk [afk #f])
  (let* ([guild (get-guild client guild-id)]
         [shard (list-ref (client-shards client) (guild-shard-id guild))])
    (send-status-update shard since game status afk)))
