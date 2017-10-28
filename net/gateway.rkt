#lang racket

(require net/rfc6455
         net/url
         racket/hash
         json
         simple-http
         "http.rkt"
         "data.rkt"
         "events.rkt"
         "client.rkt")

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

(define (new-ws-client parent shard-id)
  (ws-client null ;; ws
             (client-token parent)
             parent ;; client parent
             (string->url (get-ws-url (client-requester parent))) ;; gateway url
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

(define (send-identify client presence)
  (displayln "IDENTIFYING")
  (ws-send! (ws-client-ws client) (make-identify (ws-client-token client)
                                                 (list (ws-client-shard-id client)
                                                       (length (client-shards (ws-client-client client))))
                                                 presence)))

(define (send-resume client)
  (displayln "RESUMING")
  (ws-send! (ws-client-ws client) (make-resume (ws-client-token client)
                                               (ws-client-session-id client)
                                               (ws-client-seq client))))

(define (trigger-reconnect client)
  (disconnect client)
  (connect client))

(define (connect client)
  (set-ws-client-ws! client (ws-connect (ws-client-gateway-url client)))
  (set-ws-client-seq! client null)
  (set-ws-client-heartbeat-received! client #t)
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
               (thread-send (client-event-consumer (ws-client-client client)) (list client d t))]
              [(== op-heartbeat)
               (send-heartbeat client)]
              [(== op-reconnect)
               (trigger-reconnect client)
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
              [_ (printf "Unhandled opcode: ~a\n" o)])]
           [(? eof-object?) (println "WS GAVE EOF: RECONNECTING") (trigger-reconnect client)] ;; TODO: detect if we're looping
           [x (printf "Unhandled response: ~a\n" x)])
         (loop))
       (printf "WS LOOP ON SHARD: ~a EXITING" (ws-client-shard-id client))))))

(define (send-heartbeat client)
  (if (ws-client-heartbeat-received client)
      (ws-send! (ws-client-ws client) (make-heartbeat (ws-client-seq client)))
      (begin
        (println "WS DID NOT BEAT: RECONNECTING")
        (trigger-reconnect client)))
  (set-ws-client-heartbeat-received! client #f))

(define (heartbeater client interval)
  (printf "Starting to heartbeat at a period of ~as" interval)
  (thread
   (lambda ()
     (let loop ()
       (send-heartbeat client)
       (sleep interval)
       (loop)))))
