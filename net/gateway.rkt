#lang racket

(require net/rfc6455
         racket/hash
         json
         simple-http
         "http.rkt")

(struct ws-client
  (ws
   client
   [heartbeat-thread #:mutable]
   [seq #:mutable]))

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

(define (get-ws-url requester)
  (string-append* (dict-ref (json-response-body (gateway requester)) 'url)
                  gateway-params))

(define (connect-ws requester)
  (ws-connect (get-ws-url requester)))

(define (new-client requester)
  (ws-client (connect-ws requester)
             'nil
             null))

(define (make-heartbeat seq)
  (jsexpr->string (hasheq
                  'op op-heartbeat
                  'd seq)))

(define (json-ws-read ws)
  (match (ws-recv ws #:payload-type 'text)
    [eof eof]
    [x (string->jsexpr x)]))

(define (send-heartbeat ws seq)
  (ws-send! ws (make-heartbeat seq)))

(define (accept-hello client data)
  (set-ws-client-heartbeat-thread! (heartbeater client (/ (hash-ref data 'heartbeat_interval) 1000))))

(define (ws-loop client)
  (thread
   (lambda ()
     (let loop ()
       (match (json-ws-read (ws-client-ws client))
         [eof #f]
         [(hash-table ('op op-dispatch) ('d d) ('s s) ('t t))
          (set-ws-client-seq! s)
          (dispatch-event client d s t)] ;; TODO: decide on how we dispatch events
         [(hash-table ('op o) ('d d))
          (match o
            [op-heartbeat (send-heartbeat (ws-client-ws client) (ws-client-seq client))]
            [op-reconnect (trigger-reconnect client)] ;; TODO: implement
            [op-invalid-session (if d
                                    (send-resume client) ;; TODO: implement
                                    (send-identify client))] ;; TODO: implement
            [op-hello (accept-hello client d)]
            [op-heartbeat-ack (accept-heartbeat client)])])
       (loop)))))

(define (heartbeater client interval)
  (thread
   (lambda ()
     (let loop ()
       (send-heartbeat (ws-client-ws client) (ws-client-seq client))
       (sleep interval)
       (loop)))))
