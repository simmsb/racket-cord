#lang racket/base

(require (only-in racket/function thunk)
         (only-in racket/hash hash-union)
         (only-in racket/string string-replace string-trim)
         (only-in racket/exn exn->string)
         "data.rkt"
         "logger.rkt")

(provide add-events
         on-event
         dispatch-event)

(define (dispatch-event ws-client data type)
  (thread
   (thunk
    (log-discord-debug "DISPATCHING EVENT: ~a" type)
    (with-handlers ([exn:fail?
                     (lambda (v)
                       (log-discord-warning "Event ~a ERRORED with: ~a"
                                            type (string-trim (exn->string v))))])
      (let ([client (ws-client-client ws-client)]
            [events (client-events (ws-client-client ws-client))]
            [raw-evt (string->symbol (string-downcase (format "raw-~a" (string-replace type "_" "-"))))])
        (for ([handler (hash-ref events raw-evt null)])
          (handler ws-client client data)))))))

(define (on-event evt client callback)
  (set-client-events!
   client
   (hash-update (client-events client)
                evt
                (lambda (existing-callbacks) (cons callback existing-callbacks))
                null)))

(define (add-events client)
  (on-event 'raw-ready client event-ready)
  (on-event 'raw-user-update client event-user-update))

(define (event-ready ws-client client data)
  (let ([session-id (hash-ref data 'session_id)]
        [user (hash-ref data 'user)])
    (set-ws-client-session-id! ws-client session-id)
    (event-user-update ws-client client user)))

(define (event-user-update _ws-client client data)
  (let ([merged (hash-union (client-user client) data
                            #:combine (lambda (v1 v2) v2))])
    (set-client-user! client merged)))
