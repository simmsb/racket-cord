#lang racket

(require "client.rkt"
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

(define (make-client token shard-count)
  (let ([clnt (new-client token)])
    (set-client-shards! clnt (map (lambda (n) (new-ws-client clnt n))
                             (range shard-count)))
    clnt))

(define (start-client client)
  (map connect (client-shards client)))

(define (stop-client client)
  (map disconnect (client-shards client)))
