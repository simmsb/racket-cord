#lang racket

(require (only-in simple-http
                  update-headers
                  update-ssl
                  update-host
                  json-requester
                  json-response-body
                  json-response-headers)
         json
         net/url
         racket/string
         srfi/19
         "data.rkt"
         "patched-simple-http.rkt"
         "utils.rkt")

(provide make-discord-http
         (struct-out exn:fail:network:http:discord)
         (struct-out http-client)
         make-http-client
         discord-url
         get-ws-url
         get-ws-url-bot
         get-channel
         create-message
         create-reaction
         delete-own-reaction
         delete-user-reaction
         get-reactions
         delete-all-reactions
         edit-message
         delete-message
         bulk-delete-messages
         edit-channel-permissions
         get-channel-invites
         create-channel-invite
         delete-channel-permission
         trigger-typing-indicator)


(struct exn:fail:network:http:discord exn:fail (code json-code message) #:transparent)


(define (make-discord-http token)
  (update-headers
   (update-ssl
    (update-host json-requester "discordapp.com")
    #t)
   (list (format "Authorization: ~a" token)
         "User-Agent: DiscordBot (https://github.com/nitros12/racket-cord, 0.0)")))

(define (make-multipart req)
  (update-headers req '("Content-Type: multipart/form-data")))

(struct http-client
  (requester
   global-lock
   ratelimits))

(define (make-http-client token)
  (http-client
   (make-discord-http token)
   (make-semaphore 1)
   (make-hash)))

(define (format-route method path channel-id guild-id)
  (format "~a|~a|~a|~a" method path channel-id guild-id))

(struct route
  (method
   path
   bucket-key
   channel-id
   guild-id)
  #:transparent)

(define (make-route method #:channel-id [channel-id null] #:guild-id [guild-id null] . path-parts)
  (route
   method
   (apply discord-url path-parts)
   (format-route method (string-join path-parts) channel-id guild-id)
   channel-id guild-id))

(define (apply-route http-client route args data params)
  (let ([method (route-method route)]
        [formatted (foldl (lambda (k s)
                            (string-replace s (format "{~a}" (car k)) (~a (cdr k))))
                          (route-path route)
                          (append (list (cons "channel-id" (route-channel-id route))
                                        (cons "guild-id" (route-guild-id route)))
                                  args))])
    (method (http-client-requester http-client) formatted #:params params #:data data)))

(define gateway-params "/?v=6&encoding=json")

(define (get-ws-url http-client)
  (string->url (string-append (dict-ref (json-response-body (get (http-client-requester http-client) (discord-url "gateway"))) 'url)
                             gateway-params)))


(define (get-ws-url-bot http-client)
  (let ([resp (json-response-body (get (http-client-requester http-client) (discord-url "gateway" "bot")))])
    (values (string->url (string-append (hash-ref resp 'url) gateway-params))
            (hash-ref resp 'shards))))

(define (discord-url . parts)
  (format "/api/v6/~a" (string-join parts "/")))

(define (rfc2822->unix-seconds s)
  (time-second (date->time-utc (string->date s "~a, ~d ~b ~Y ~H:~M:~S"))))

(define (run-route route http-client [args null] #:data [data #f] #:params [params null])
  (let ([route-key (route-bucket-key route)]
        [global-lock (http-client-global-lock http-client)]
        [ratelimits (http-client-ratelimits http-client)])
    (semaphore-wait global-lock)
    (semaphore-post global-lock)
    (let ([lock (hash-ref! ratelimits route-key (thunk (make-semaphore 1)))])
      (call-with-semaphore
       lock
       (thunk
        (let retry ([tries 5])
          (when (tries . <= . 0)
            (error 'run-route "Ran out of attempts on route: ~a" route-key))
          (with-handlers ([exn:fail:network:http:error?
                           (lambda (e)
                             (match e [(exn:fail:network:http:error _ _ code type headers body)
                                       (case code
                                         [(502) (sleep (* 2 (6 . - . tries))) (retry (sub1 tries))]
                                         [(429) (let ([retry-after ((string->number (car (hash-ref headers 'Retry-After))) . / . 1000)]
                                                      [globally (hash-ref body 'global #f)])
                                                  (log-discord-info "Hit 429 ratelimit ~a retrying in ~a seconds" route-key retry-after)
                                                  (if globally
                                                      (call-with-semaphore global-lock (thunk (sleep retry-after)))
                                                      (sleep retry-after)))
                                                (retry (sub1 tries))]
                                         [else (raise (exn:fail:network:http:discord (format "Discord gave us an error: ~a" body)
                                                                                     (current-continuation-marks) code
                                                                                     (hash-ref body 'code null)
                                                                                     (hash-ref body 'message null)))])]))])
            (let* ([resp (apply-route http-client route args data params)]
                   [resp-headers (json-response-headers resp)]
                   [remaining (string->number (car (hash-ref resp-headers 'X-Ratelimit-Remaining '("0"))))]
                   [reset (string->number (car (hash-ref resp-headers 'X-Ratelimit-Reset '("0"))))]
                   [date (rfc2822->unix-seconds (car (hash-ref resp-headers 'Date)))])
              (when (remaining . <= . 0)
                (let ([delta (max 0 (reset . - . date))])
                  (log-discord-info "Sleeping on ratelimit ~a for ~a seconds" route-key delta)
                  (sleep delta)))
              (json-response-body resp)))))))))

(define (get-channel client id)
  (run-route (make-route put "channels" "{channel-id}" #:channel-id id) (client-http-client client)))

(define (create-message client channel-id content #:embed [embed null] #:tts [tts #f])
  (let ([data (make-hash)])
    (unless (null? embed)
      (hash-set! data 'embed embed))
    (hash-set! data 'tts tts)
    (hash-set! data 'content content)
    (hash->message (run-route (make-route post "channels" "{channel-id}" "messages" #:channel-id channel-id)
                             (client-http-client client) #:data (jsexpr->string data)))))

(define (create-reaction client channel-id message-id emoji)
  (run-route (make-route post "channels" "{channel-id}" "messages" "{message-id}" "reactions" "{emoji}" "@me"
                         #:channel-id channel-id)
             (client-http-client client) `((message-id . ,message-id) (emoji . ,emoji))))

(define (delete-own-reaction client channel-id message-id emoji)
  (run-route (make-route delete "channels" "{channel-id}" "messages" "{message-id}" "reactions" "{emoji}" "@me"
                         #:channel-id channel-id)
             (client-http-client client) `((message-id . ,message-id) (emoji . ,emoji))))

(define (delete-user-reaction client channel-id message-id emoji user-id)
  (run-route (make-route delete "channels" "{channel-id}" "messages" "{message-id}" "reactions" "{emoji}" "{user-id}"
                         #:channel-id channel-id)
             (client-http-client client) `((message-id . ,message-id) (emoji . ,emoji) (user-id . ,user-id))))

(define (get-reactions client channel-id message-id emoji)
  (run-route (make-route get "channels" "{channel-id}" "messages" "{message-id}" "reactions" "{emoji}"
                         #:channel-id channel-id)
             (client-http-client client) `((message-id . ,message-id) (emoji . ,emoji))))

(define (delete-all-reactions client channel-id message-id)
  (run-route (make-route delete "channels" "{channel-id}" "messages" "{message-id}"
                         #:channel-id channel-id)
             (client-http-client client) `((message-id . ,message-id))))

(define (edit-message client channel-id message-id)
  (run-route (make-route patch "channels" "{channel-id}" "messages" "{message-id}"
                         #:channel-id channel-id)
             (client-http-client client) `((message-id . ,message-id))))

(define (delete-message client channel-id message-id)
  (run-route (make-route delete "channels" "{channel-id}" "messages" "{message-id}"
                         #:channel-id channel-id)
             (client-http-client client) `((message-id . ,message-id))))

(define (bulk-delete-messages client channel-id . ids)
  (run-route (make-route post "channels" "{channel-id}" "messages" "bulk-delete"
                         #:channel-id channel-id)
             (client-http-client client) #:data (jsexpr->string (hash 'messages ids))))

(define (edit-channel-permissions client channel-id overwrite-id allow deny type)
  (run-route (make-route put "channels" "{channel-id}" "permissions" "{overwrite-id}"
                         #:channel-id channel-id)
             (client-http-client client) `((overwrite-id . ,overwrite-id))
             #:data (jsexpr->string
                     (hash 'allow allow
                           'deny deny
                           'type type))))


(define (get-channel-invites client channel-id)
  (run-route (make-route get "channels" "{channel-id}" "invites"
                         #:channel-id channel-id)
             (client-http-client client)))

(define (create-channel-invite client channel-id [age 86400] [uses 0] [temporary #f] [unique #f])
  (hash->invite
   (run-route (make-route post "channels" "{channel-id}" "invites"
                          #:channel-id channel-id)
              (client-http-client client) #:data (jsexpr->string
                                                  (hash 'max_age age
                                                        'max_uses uses
                                                        'temporary temporary
                                                        'unique unique)))))

(define (delete-channel-permission client channel-id overwrite-id)
  (run-route (make-route delete "channels" "{channel-id}" "permissions" "{overwrite-id}"
                         #:channel-id channel-id)
             (client-http-client client) `((overwrite-id . ,overwrite-id))))

(define (trigger-typing-indicator client channel-id)
  (run-route (make-route post "channels" "{channel-id}" "typing"
                         #:channel-id channel-id)
             (client-http-client client)))
