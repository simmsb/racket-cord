#lang racket

(require racket/set
         "data.rkt"
         "events.rkt"
         "http.rkt")

(provide new-client
         make-request)

(define (new-client token)
  (let ([clnt
         (client
          null
          null
          (make-hash)
          (make-hash)
          (event-consumer)
          (make-hash)
          (make-discord-http token)
          (http-request-loop)
          token)])
    (add-events clnt)
    clnt))

(define (make-request client endpoint . args) ;; make a request on the http loop
  (thread-send (client-http-loop client) (list endpoint (current-thread) args))
  (thread-receive))
