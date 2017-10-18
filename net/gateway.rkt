#lang racket

(require net/rfc6455
         json
         simple-http
         "http.rkt")

(struct client-ws
  (ws
   heartbeat-thread
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
  (client-ws (connect-ws requester)
             'nil
             null))

(define (make-heartbeat seq)
  (jsexpr->string (hasheq
                  'op op-heartbeat
                  'd seq)))

(define (heartbeater ws interval)
  (thread
   (lambda ()
     (let loop ()
       (ws-send! (client-ws-ws ws) (make-heartbeat (client-ws-seq ws)))
       (sleep interval)
       (loop)))))
