#lang racket

(require net/rfc6455
         racket/hash
         json
         simple-http
         "http.rkt")

(struct ws-client
  (ws
   token
   [gateway-url #:mutable]
   [heartbeat-thread #:mutable]
   [heartbeat-delta #:mutable]
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

(define (startup-ws url)
  (ws-connect url))

(define (new-ws-client token)
  (let ([requester (make-discord-http token)]
        [url (get-ws-url requester)])
    (ws-client (startup-ws url)
               token
               url
               'nil
               0
               'null)))


(define (make-heartbeat seq)
  (jsexpr->string (hasheq
                  'op op-heartbeat
                  'd seq)))

(define (json-ws-read ws)
  (let ([data (ws-recv ws #:payload-type 'text)])
    (displayln data) ;; DEBUG
    (match data
      [eof eof]
      [x (string->jsexpr x)])))

(define (send-heartbeat ws seq)
  (ws-send! ws (make-heartbeat seq)))

(define (accept-hello client data)
  (set-ws-client-heartbeat-thread! (heartbeater client (/ (hash-ref data 'heartbeat_interval) 1000))))

(define (make-identify token) ;; TODO: more args
  (hasheq
   'token token
   'properties (hasheq))
  )

(define (send-identify client) 'nil)

(define (send-resume client) 'nil)

(define (accept-heartbeat client) 'nil)

(define (trigger-reconnect client) 'nil)

(define (dispatch-event cleint d s t) 'nil)

(define (ws-loop client)
  (thread
   (lambda ()
     (send-identify client)
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
                                    (begin
                                      (sleep (random 1 5))
                                      (send-identify client)))]
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
