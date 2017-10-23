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
    (for ([guild guilds])
      (dict-set! (ws-client-guilds ws-client) (hash-ref guild 'id) (hash->guild guild))) ;; these are only partials idk
    (for ([priv private-channels])
      (dict-set! (ws-client-private-channels) (hash-ref priv 'id) (hash->channel priv)))))

(define (event-guild-create ws-client data)
  (dict-set! (ws-client-guilds ws-client) (hash-ref data 'id) (hash->guild guild)))

(define (dispatch-event ws-client data type)
  (printf "DISAPTCHING EVENT: ~a" type)
  (case type
    [("READY") (thread (lambda () (event-ready ws-client data)))]
    [("GUILD_CREATE") (thread (lambda () (event-guild-create ws-client data)))]))
