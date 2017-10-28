#lang racket

(require "data.rkt"
         "utils.rkt")

(provide event-ready
         event-guild-create
         dispatch-event
         event-consumer
         add-events)

(define (event-ready ws-client data)
  (let ([client (ws-client-client ws-client)]
        [session-id (hash-ref data 'session_id)]
        [private-channels (hash-ref data 'private_channels)]
        [guilds (hash-ref data 'guilds)]
        [user (hash-ref data 'user)])
    (set-ws-client-session-id! ws-client session-id)
    (when (null? (client-user client))
      (set-client-user! client (hash->user user)))
    (for ([guild-obj guilds])
      (hash-set! (client-guilds client) (hash-ref guild-obj 'id)
                 (hash->guild (ws-client-shard-id ws-client) guild-obj))) ;; these are only partials idk
    (for ([priv private-channels])
      (hash-set! (client-private-channels client) (hash-ref priv 'id) (hash->channel priv)))))

(define (event-guild-create ws-client data)
  (hash-set! (client-guilds (ws-client-client ws-client)) (hash-ref data 'id) (hash->guild (ws-client-shard-id ws-client) data)))

(define (dispatch-event ws-client data type)
  (printf "DISAPTCHING EVENT: ~a ~a" type data)
  (let ([client (ws-client-client ws-client)]
        [events (client-events (ws-client-client ws-client))]
        [raw-evt (string->symbol (string-downcase (format "raw-~a" (string-replace type "_" "-"))))]
        [evt (string->symbol (string-downcase (string-replace type "_" "-")))])
    (match (hash-ref events raw-evt null) ;; Apply raw events TODO: move this to after non-raw events
      [(? null?) null]
      [funs (each funs ws-client data type)])
    (match (hash-ref events evt null)
      [(? null?) null]
      [funs
       (case evt
         [(ready) (each funs client)]
         [(channel-create channel-delete) (each funs client (hash->channel data))]
         ;;[(channel-update) (each funs client ))] TODO: THIS
         ;;[(guild-update) (each funs client
         ;;                      (hash-ref (ws-client-guilds ws-client) (hash-ref data 'id))
         ;;                      (hash->guild (ws-client-shard-id ws-client) data))]

)])))

(define (event-consumer)
  (thread
   (lambda ()
     (let loop ()
       (match (thread-receive)
         [(list ws data type)
          (dispatch-event ws data type)
          (loop)]
         ['done null])))))

(define (on-event evt client callback)
  (let ([events (client-events client)])
        (match (hash-ref events evt null)
          [(? null?) (hash-set events evt (mutable-set callback))]
          [x (set-add! x callback)])))

(define (add-events client)
  (on-event 'raw-ready client event-ready)
  (on-event 'raw-guild-create client event-ready))
