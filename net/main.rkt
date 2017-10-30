#lang racket

(require "client.rkt"
         "data.rkt"
         "events.rkt"
         "gateway.rkt"
         "http.rkt"
         "utils.rkt")

(provide make-client
         start-client
         stop-client)

(define (make-client token shard-count)
  (let ([clnt (new-client token)])
    (set-client-shards! clnt (map (lambda (n) (new-ws-client clnt n))
                             (range shard-count)))
    clnt))

(define (start-client client)
  (map connect (client-shards client)))

(define (stop-client client)
  (map disconnect (client-shards client)))
