#lang racket

(require net/rfc6455
         racket/hash
         json
         (only-in "http.rkt"
                  get-ws-url)
         "data.rkt"
         "events.rkt"
         "utils.rkt")

(provide new-ws-client
         start-shard
         stop-shard
         send-request-guild-members
         send-presence-update)

(define op-dispatch  0)
(define op-heartbeat 1)
(define op-identifiy 2)
(define op-presence-update 3)
(define op-voice-state-update 4)
(define op-resume 6)
(define op-reconnect 7)
(define op-request-guild-members 8)
(define op-invalid-session 9)
(define op-hello 10)
(define op-heartbeat-ack 11)

#|
Architecture:
- Every ws-client instance represents one shard, and is associated with one permanently
  running master thread that is only terminated when the entire shard is stopped.
- Every ws-client is further associated with at most one receive thread and at most
  one heartbeater thread at all times. These threads are treated as cheap and transient.
- Whenever the receive thread or heartbeater thread notice something is wrong, they send
  a pair '("error msg" . resumable) to the master thread via the stop-channel.
- When the master thread notices either a value on the stop-channel (one of the threads
  asked to reconnect), or one of the threads died (unexpected crash), it will clean up,
  close the socket, sleep briefly, then start another connection.
- If the error is specified to be nonresumable, the session and sequence numbers are cleared to force
  a fresh IDENTIFY handshake instead of a RESUME next reconnection.
- The ws-client does not hold onto its master thread, the caller (upper level discord client)
  is the one responsible for terminating it by sending it a break.
|#

(define (new-ws-client parent shard-id ws-url)
  (ws-client (client-token parent)
             parent
             shard-id

             ws-url
             #f
             #f
             #f
             #f
             #f

             #f
             #f))

(define (json-ws-read ws)
  (let ([data (ws-recv ws #:payload-type 'text)])
    (if (string? data)
        (string->jsexpr data #:null #f)
        eof)))

(define (send-identify client presence)
  (define (make-identify token shard presence intents)
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
          'v 8
          'intents (foldl bitwise-ior 0 intents)
          'shard shard
          'presence presence))))
  (log-discord-debug "IDENTIFYING")
  (ws-send! (ws-client-socket client)
            (make-identify (ws-client-token client)
                           (list (ws-client-shard-id client)
                                 (length (client-shards (ws-client-client client))))
                           presence
                           (client-intents (ws-client-client client)))))

(define (send-resume client)
  (define (make-resume token session-id seq)
    (jsexpr->string
     (hasheq
      'op op-resume
      'd (hasheq
          'token token
          'session_id session-id
          'seq seq))))
  (log-discord-debug "RESUMING")
  (ws-send! (ws-client-socket client)
            (make-resume (ws-client-token client)
                         (ws-client-session-id client)
                         (ws-client-seq client))))

(define (start-shard client)
  (thread
   (thunk
    (with-handlers ([exn:break? (lambda (x)
                                  (log-discord-info "Master gateway thread for shard ~a broken, terminating"
                                                    (ws-client-shard-id client))
                                  (disconnect client)
                                  (void))])
      (let loop ()
        (connect client)

        ; wait for a known session termination or unexpected death of one of the threads
        (let* ([result (sync (ws-client-heartbeat-thread client)
                            (ws-client-recv-thread client)
                            (ws-client-stop-channel client))]
               [resumable (match result
                            [(== (ws-client-heartbeat-thread client))
                             (log-discord-error "Heartbeat thread died unexpectedly")
                             #t]
                            [(== (ws-client-recv-thread client))
                             (log-discord-error "Receive thread died unexpectedly")
                             #t]
                            [(cons errmsg resumable)
                             (log-discord-error "Error: ~a" errmsg)
                             resumable])])

          (disconnect client)
          (when (not resumable)
              (set-ws-client-seq! client #f)
              (set-ws-client-session-id! client #f))
          (sleep (random 1 5))
          (loop)))))))

(define (stop-shard thd)
  (break-thread thd 'terminate))

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
  (set-ws-client-socket! client (make-ws-conn))
  (set-ws-client-seq! client #f)
  (set-ws-client-heartbeat-acked! client #t)
  (set-ws-client-stop-channel! client (make-channel))
  (set-ws-client-recv-thread! client (ws-loop client))
  (set-ws-client-heartbeat-thread! client (heartbeater client)))

(define (disconnect client #:status [status 1000] #:reason [reason ""])
  (when (ws-client-heartbeat-thread client)
    (kill-thread (ws-client-heartbeat-thread client)))
  (set-ws-client-heartbeat-thread! client #f)
  (set-ws-client-heartbeat-acked! client #f)

  (when (ws-client-recv-thread client)
    (kill-thread (ws-client-recv-thread client)))
  (set-ws-client-recv-thread! client #f)

  (when (ws-client-socket client)
    (ws-close! (ws-client-socket client)
               #:status status
               #:reason reason))
  (set-ws-client-socket! client #f)

  (set-ws-client-stop-channel! client #f))
; keep session id, sequence number for resuming

(define (ws-loop client)
  (thread
   (thunk
    (let loop ()
      (let ([data (json-ws-read (ws-client-socket client))])
        (if (eof-object? data)
            (channel-put (ws-client-stop-channel client) '("Received EOF on websocket" . #t))
            (let ([op (hash-ref data 'op)]
                  [d (hash-ref data 'd #f)]
                  [seq (hash-ref data 's #f)]
                  [event-name (hash-ref data 't #f)])
              (match op
                [(== op-dispatch)
                 (set-ws-client-seq! client seq)
                 (dispatch-event client d event-name)]
                [(== op-heartbeat)
                 (send-heartbeat client)]
                [(== op-reconnect)
                 (channel-put (ws-client-stop-channel client) '("Received opcode reconnect" . #t))]
                [(== op-invalid-session)
                 (channel-put (ws-client-stop-channel client) '("Received invalid session" . d))]
                [(== op-hello)
                 (thread-send (ws-client-heartbeat-thread client)
                              (/ (hash-ref data 'heartbeat_interval) 1000))
                 (if (and (ws-client-seq client) (ws-client-session-id client))
                     (send-resume client)
                     (send-identify client (hash)))] ;; TODO: sort out presence passing
                [(== op-heartbeat-ack)
                 (set-ws-client-heartbeat-acked! client #t)]
                [_ (log-discord-debug "Unhandled opcode: ~a\n" op)]))))
      (loop))
    (log-discord-debug "WS LOOP ON SHARD: ~a EXITING" (ws-client-shard-id client)))))

(define (send-heartbeat client)
  (define (make-heartbeat seq)
    (jsexpr->string (hasheq
                     'op op-heartbeat
                     'd seq)))
  (if (ws-client-heartbeat-acked client)
      (begin
        (ws-send! (ws-client-socket client) (make-heartbeat (ws-client-seq client)))
        (set-ws-client-heartbeat-acked! client #f))
      (channel-put (ws-client-stop-channel client) '("Unacked heartbeat" . #t))))

(define (heartbeater client)
  (thread
   (thunk
    (let ([interval (thread-receive)])
      (log-discord-debug "Starting to heartbeat at a period of ~a sec" interval)
      (let loop ()
        (send-heartbeat client)
        (sleep interval)
        (loop))))))

; user-facing gateway queries

(define (send-request-guild-members client guild-id #:query [query ""] #:limit [limit 0])
  (log-discord-debug "REQUESTING GUILD MEMBERS")
  (ws-send! (ws-client-socket client)
            (jsexpr->string
             (hasheq
              'op op-request-guild-members
              'd (hasheq
                  'guild_id guild-id
                  'query query
                  'limit 'limit)))))

(define (send-presence-update client since game status afk)
  (log-discord-debug "SENDING STATUS UPDATE")
  (ws-send! (ws-client-socket client)
            (jsexpr->string
             (hasheq
              'op op-presence-update
              'd (hasheq
                  'since (if (not since) (json-null) since)
                  'game (game->hash game)
                  'status status
                  'afk afk)))))
