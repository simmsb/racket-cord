#lang racket

(require "data.rkt"
         "client.rkt")

(provide event-ready
         event-guild-create
         dispatch-event)

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
      (dict-set! (ws-client-guilds ws-client) (hash-ref guild-obj 'id) (hash->guild guild-obj))) ;; these are only partials idk
    (for ([priv private-channels])
      (dict-set! (ws-client-private-channels) (hash-ref priv 'id) (hash->channel priv)))))

(define (event-guild-create ws-client data)
  (dict-set! (ws-client-guilds ws-client) (hash-ref data 'id) (hash->guild data)))

(define (dispatch-event ws-client data type)
  (printf "DISAPTCHING EVENT: ~a ~a" type data)
  (case type
    [("READY") (event-ready ws-client data)]
    [("GUILD_CREATE") (event-guild-create ws-client data)]))

(define (event-consumer client)
  (thread
   (lambda ()
     (let loop ()
       (match (thread-receive)
         [(list ws data type)
          (dispatch-event ws data type)
          (loop)]
         ['done null])))))
