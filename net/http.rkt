#lang racket

(require simple-http
         net/url
         racket/string)

(provide make-discord-http
         discord-url
         get-gateway
         get-gateway-bot
         get-ws-url
         get-ws-url-bot
         create-message)

(define (make-discord-http token)
  (update-headers
   (update-ssl
    (update-host json-requester "discordapp.com")
    #t)
   (list (format "Authorization: ~a" token)
         "User-Agent: DiscordBot (https://github.com/nitros12/racket-cord, 0.0)")))

(struct http-client
  (requester
   global-lock
   ratelimits))

(struct bucket
  (route
   semaphore
  [remaining #:mutable]
  reset))

(define (create-bucket hash route remaining reset)
  (let ([new-bucket (bucket route
                            (make-semaphore 1)
                            remaining
                            reset)])
    (hash-set! hash route new-bucket)
    new-bucket))

(define gateway-params "/?v=6&encoding=json")

(define (get-ws-url requester)
  (string->url (string-append (dict-ref (json-response-body (get-gateway requester)) 'url)
                             gateway-params)))

(define (get-ws-url-bot requester)
  (let ([resp (json-response-body (get-gateway-bot requester))])
    (values (string->url (string-append (hash-ref resp 'url) gateway-params))
            (hash-ref resp 'shards))))

(define (discord-url . parts)
  (format "/api/v6/~a" (string-join parts "/")))

(define (get-gateway requester)
  (get requester (discord-url "gateway")))

(define (get-gateway-bot requester)
  (get requester (discord-url "gateway" "bot")))

(define (create-message requester ch-id payload)
  (post requester (discord-url "channels" (number->string ch-id) "messages")
        #:data payload))


(define (run-route route-key http-client proc)
  (let ([global-lock (http-client-global-lock http-client)]
        [ratelimits (http-client-ratelimits http-client)])
    (semaphore-wait global-lock)
    (semaphore-post global-lock)
    (unless (hash-has-key? ratelimits route-key)
      (hash-set! ratelimits route-key (make-semaphore 1)))
    (let ([lock (hash-ref ratelimits route-key)])
      (semaphore-wait lock)
      (proc)
      (semaphore-post lock)
      (when )))) ;; immediately delete the lock after we use it

;; TODO: all of http

(define (request-route route http-client . args)
  (run-route route route-hash
             (thunk
              (let retry ([])))))
