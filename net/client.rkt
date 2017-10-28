#lang racket

(require racket/set
         "data.rkt"
         "events.rkt"
         "http.rkt")

(provide make-client
         make-request)

(define (make-client token shards)
  (add-events
   (client shards
           null
           (make-hash)
           (make-hash)
           (event-consumer)
           (make-hash)
           (make-discord-http token)
           (http-request-loop)
           token)))

(define (make-request client endpoint . args) ;; make a request on the http loop
  (thread-send (client-http-loop client) (list endpoint (current-thread) args))
  (thread-receive))
