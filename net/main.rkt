#lang racket

(require json
         "data.rkt"
         "events.rkt"
         "gateway.rkt"
         "http.rkt"
         "utils.rkt")

(provide make-client
         start-client
         stop-client
         on-event
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
                     #:token-type [token-type 'bot]
                     #:auto-shard [auto-shard #f]
                     #:shard-count [shard-count 1])
  (let ([fmt-token (format-token token token-type)])
    (let ([client (new-client fmt-token)])
      (let-values ([(ws-url shards)
                    (if auto-shard
                        (get-ws-url-bot (client-requester client))
                        (values (get-ws-url (client-requester client))
                                shard-count))])
        (set-client-shards! client (map (lambda (n) (new-ws-client client n ws-url))
                                        (range shards))))
      client)))

(define (new-client token)
  (let ([clnt
         (client
          null
          null
          (make-hash)
          (make-hash)
          (make-hash)
          (make-discord-http token)
          (http-request-loop)
          token
          (make-semaphore 1))])
    (add-events clnt)
    clnt))

;; make a request on the http loop eventually this will be used to to ratelimiting
(define (make-request client endpoint . args)
  (thread-send (client-http-loop client) (list endpoint (current-thread) args))
  (thread-receive))


(define (start-client client)
  (semaphore-wait (client-running client))
  (for ([shard (client-shards client)])
    (connect shard)))

(define (start-client-no-wait client)
  (for ([shard (client-shards client)])
    (connect shard)))

(define (stop-client client)
  (for ([shard (client-shards client)])
    (disconnect shard))
  (semaphore-post (client-running client)))
