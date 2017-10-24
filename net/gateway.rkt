#lang racket

(require net/rfc6455
         net/url
         racket/hash
         json
         simple-http
         "http.rkt"
         "data.rkt"
         "events.rkt")

(provide new-ws-client
         connect
         disconnect)


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

(define gateway-params "/?v=6&encoding=json")

(define (get-ws-url req)
  (string-append (dict-ref (json-response-body (get-gateway req)) 'url)
                 gateway-params))

(define (new-ws-client parent token shard-id)
  (let ([req (make-discord-http token)])
    (ws-client null ;; ws
               token
               parent ;; client parent
               (string->url (get-ws-url req)) ;; gateway url
               shard-id
               (make-hash) ;; guilds
               (make-hash) ;; private-channels
               #f ;; ready
               null ;; session-id
               null ;; heartbeat-thread
               null ;; recv-thread
               0
               null)))


(define (make-heartbeat seq)
  (jsexpr->string (hasheq
                  'op op-heartbeat
                  'd seq)))

(define (json-ws-read ws)
  (let ([data (ws-recv ws #:payload-type 'text)])
    (unless (eof-object? data) (println data))
    (if (string? data)
        (string->jsexpr data)
        eof)))

(define (accept-hello client data)
  (set-ws-client-heartbeat-thread! client (heartbeater client (/ (hash-ref data 'heartbeat_interval) 1000))))

(define (make-identify token shard presence) ;; TODO: more args
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
        'shard shard))))

(define (make-resume token session-id seq)
  (jsexpr->string
   (hasheq
    'op op-resume
    'd (hasheq
        'token token
        'session_id session-id
        'seq seq))))

(define (send-identify client)
  (displayln "IDENTIFYING")
  (ws-send! (ws-client-ws client) (make-identify (ws-client-token client)
                                                 (ws-client-shard-id client)
                                                 (hasheq))))

(define (send-resume client)
  (displayln "RESUMING")
  (ws-send! (ws-client-ws client) (make-resume (ws-client-token client)
                                               (ws-client-session-id client)
                                               (ws-client-seq client))))

(define (accept-heartbeat client)
  (set-ws-client-heartbeat-delta! client (sub1 (ws-client-heartbeat-delta client))))

(define (trigger-reconnect client)
  (disconnect client)
  (connect client))

(define (connect client)
  (set-ws-client-ws! client (ws-connect (ws-client-gateway-url client)))
  (set-ws-client-seq! client null)
  (set-ws-client-heartbeat-delta! client 0)
  (set-ws-client-recv-thread! client (ws-loop client)))

(define (disconnect client)
  (kill-thread (ws-client-heartbeat-thread client))
  (set-ws-client-heartbeat-thread! client null)
  (ws-close! (ws-client-ws client)))

(define (ws-loop client)
  (thread
   (lambda ()
     (let exit ()
       (let loop ()
         (match (json-ws-read (ws-client-ws client))
           [(hash-table ('op o) ('d d) ('s s) ('t t))
            (match o
              [(== op-dispatch)
               (set-ws-client-seq! client s)
               (dispatch-event client d t)]
              [(== op-heartbeat)
               (send-heartbeat (ws-client-ws client) (ws-client-seq client))]
              [(== op-reconnect)
               (trigger-reconnect client)
               (exit)]
              [(== op-invalid-session)
               (if d
                   (send-resume client)
                   (begin (sleep (random 1 5))
                          (send-identify client)))]
              [(== op-hello)
               (accept-hello client d)
               (send-identify client)]
              [(== op-heartbeat-ack)
               (accept-heartbeat client)]
              [_ (printf "Unhandled opcode: ~a\n" o)])]
           [(? eof-object? _)  (sleep (/ 1 100)) #f] ;; TODO: check if we get EOF's only when WS is killed
           [x (printf "Unhandled response: ~a\n" x)])
         (loop))))))

(define (send-heartbeat ws seq)
  (ws-send! ws (make-heartbeat seq)))

(define (heartbeater client interval)
  (printf "Starting to heartbeat at a period of ~as" interval)
  (thread
   (lambda ()
     (let loop ()
       (send-heartbeat (ws-client-ws client) (ws-client-seq client))
       (sleep interval)
       (loop)))))
