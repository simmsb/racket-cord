#lang racket

(require net/rfc6455
         racket/hash
         json
         simple-http
         (only-in "http.rkt"
                  get-ws-url)
         "data.rkt"
         "events.rkt"
         "utils.rkt")

(provide new-ws-client
         connect
         disconnect
         send-request-guild-members
         send-status-update)


(define op-dispatch  0)
(define op-heartbeat 1)
(define op-identifiy 2)
(define op-status-update 3)
(define op-voice-state-update 4)
(define op-voice-server-ping 5)
(define op-resume 6)
(define op-reconnect 7)
(define op-request-guild-members 8)
(define op-invalid-session 9)
(define op-hello 10)
(define op-heartbeat-ack 11)


(define (new-ws-client parent shard-id ws-url)
  (ws-client null ;; ws
             (client-token parent)
             parent ;; client parent
             ws-url
             shard-id
             #f ;; ready
             null ;; session-id
             null ;; heartbeat-thread
             null ;; recv-thread
             #t
             null))


(define (make-heartbeat seq)
  (jsexpr->string (hasheq
                  'op op-heartbeat
                  'd seq)))

(define (json-ws-read ws)
  (let ([data (ws-recv ws #:payload-type 'text)])
    (if (string? data)
        (string->jsexpr data)
        eof)))

(define (accept-hello client data)
  (set-ws-client-heartbeat-thread! client (heartbeater client (/ (hash-ref data 'heartbeat_interval) 1000))))

(define (make-identify token shard presence)
  (jsexpr->string
   (hasheq
    'op op-identifiy
    'd (hasheq
        'token token
        'properties (hasheq
                     '$os (symbol->string (system-type 'os))
                     '$browser "racket-cord"
                     '$device "racket-cord")
        'compress #f
        'large_threshold 50
        'v 6
        'shard shard
        'presence presence))))

(define (make-resume token session-id seq)
  (jsexpr->string
   (hasheq
    'op op-resume
    'd (hasheq
        'token token
        'session_id session-id
        'seq seq))))

(define (send-request-guild-members client guild-id #:query [query ""] #:limit [limit 0])
  (log-discord-debug "REQUESTING GUILD MEMBERS")
  (ws-send! (ws-client-ws client) (jsexpr->string
                                   (hasheq
                                    'op op-request-guild-members
                                    'd (hasheq
                                        'guild_id guild-id
                                        'query query
                                        'limit 'limit)))))

(define (send-status-update client since game status afk)
  (log-discord-debug "SENDING STATUS UPDATE")
  (ws-send! (ws-client-ws client) (jsexpr->string
                                   (hasheq
                                    'op op-status-update
                                    'd (hasheq
                                        'since since
                                        'game (game->hash game)
                                        'status status
                                        'afk afk)))))

(define (send-identify client presence)
  (log-discord-debug "IDENTIFYING")
  (ws-send! (ws-client-ws client) (make-identify (ws-client-token client)
                                                 (list (ws-client-shard-id client)
                                                       (length (client-shards (ws-client-client client))))
                                                 presence)))

(define (send-resume client)
  (log-discord-debug "RESUMING")
  (ws-send! (ws-client-ws client) (make-resume (ws-client-token client)
                                               (ws-client-session-id client)
                                               (ws-client-seq client))))

(define (trigger-reconnect client [reason ""])
  (disconnect client #:reason reason)
  (connect client))

(define (connect client)
  (define (make-ws-conn)
    (let retry ([count 0]
                [backoff 1])
      (if (> count 3)
          (begin (set-ws-client-gateway-url! client (get-ws-url (client-http-client (ws-client-client client)))) ;; This is pretty bad
                 (log-discord-debug "Failed to connect to gateway more than 3 times, getting a new url.")
                 (sleep 5)
                 (make-ws-conn))
          (with-handlers ([exn:fail:network?
                           (lambda (e)
                             (begin (sleep backoff)
                                    (retry (add1 count)
                                           (* 2 backoff))))])
            (ws-connect (ws-client-gateway-url client))))))
  (set-ws-client-ws! client (make-ws-conn))
  (set-ws-client-seq! client null)
  (set-ws-client-heartbeat-received! client #t)
  (set-ws-client-recv-thread! client (ws-loop client)))

(define (disconnect client #:status [status 1000] #:reason [reason ""])
  (unless (null? (ws-client-heartbeat-thread client))
    (kill-thread (ws-client-heartbeat-thread client)))
  (set-ws-client-heartbeat-thread! client null)
  (ws-close! (ws-client-ws client)
             #:status status
             #:reason reason))

(define (ws-loop client)
  (thread
   (thunk
    (let exit ()
      (let loop ()
        (let ([data (json-ws-read (ws-client-ws client))])
          (if (eof-object? data)
              (begin (log-discord-debug "WS GAVE EOF: Reconnecting")
                     (trigger-reconnect client "WS Closed."))
              (let ([o (hash-ref data 'op null)]
                    [d (hash-ref data 'd null)]
                    [s (hash-ref data 's null)]
                    [t (hash-ref data 't null)])
                (match o
                  [(== op-dispatch)
                   (set-ws-client-seq! client s)
                   (dispatch-event client d t)]
                  [(== op-heartbeat)
                   (send-heartbeat client)]
                  [(== op-reconnect)
                   (trigger-reconnect client "Recieved reconnect payload.")
                   (exit)]
                  [(== op-invalid-session)
                   (if d
                       (send-resume client)
                       (begin (sleep (random 1 5))
                              (send-identify client (hash))))]
                  [(== op-hello)
                   (accept-hello client d)
                   (send-identify client (hash))] ;; TODO: sort out presence passing
                  [(== op-heartbeat-ack)
                   (set-ws-client-heartbeat-received! client #t)]
                  [_ (log-discord-debug "Unhandled opcode: ~a\n" o)]))))
        (loop))
      (log-discord-debug "WS LOOP ON SHARD: ~a EXITING" (ws-client-shard-id client))))))

(define (send-heartbeat client)
  (if (ws-client-heartbeat-received client)
      (ws-send! (ws-client-ws client) (make-heartbeat (ws-client-seq client)))
      (begin
        (log-discord-debug "WS DID NOT BEAT: RECONNECTING")
        (trigger-reconnect client "Did not recieve heartbeat ACK.")))
  (set-ws-client-heartbeat-received! client #f))

(define (heartbeater client interval)
  (log-discord-debug "Starting to heartbeat at a period of ~as" interval)
  (thread
   (thunk
    (let loop ()
      (send-heartbeat client)
      (sleep interval)
      (loop)))))
