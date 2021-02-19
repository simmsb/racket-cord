#lang racket

(require json
         "constants.rkt"
         "data.rkt"
         "events.rkt"
         "gateway.rkt"
         (prefix-in http: "http.rkt")
         "utils.rkt")

(provide make-client
         start-client
         start-client-no-wait
         stop-client
         on-event
         update-status
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

(define (start-client client)
  (for ([shard (client-shards client)])
    (connect shard))
  (semaphore-wait (client-running client)))


(define (start-client-no-wait client)
  (for ([shard (client-shards client)])
    (connect shard)))

(define (stop-client client)
  (for ([shard (client-shards client)])
    (disconnect shard))
  (semaphore-post (client-running client)))

(define (update-status client guild-id
                       #:since [since null]
                       #:game [game null]
                       #:status [status "online"]
                       #:afk [afk #f])
  (let* ([guild (get-guild client guild-id)]
         [shard (list-ref (client-shards client) (guild-shard-id guild))])
    (send-status-update shard since game status afk)))
