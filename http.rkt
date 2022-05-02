#lang racket/base

(require net/http-easy
         json
         net/url
         racket/contract
         racket/format
         racket/function
         racket/match
         "private/data.rkt"
         "private/logger.rkt"
         "private/utils.rkt"
         (for-syntax racket/base syntax/parse syntax/datum))

(provide make-http-client
         get-ws-url
         get-ws-url-infallible
         get-ws-url-bot
         (struct-out exn:fail:network:http:discord)
         (contract-out
          [struct attachment
            ([data bytes?]
             [type (or/c string? bytes?)]
             [name (or/c string? bytes?)])]))

(struct exn:fail:network:http:discord exn:fail (http-code discord-code reason) #:transparent)
(struct attachment (data type name) #:transparent)

(define (make-discord-http token)
  (make-keyword-procedure
   (lambda (kws kwargs req uri #:headers [headers (hasheq)])
     (keyword-apply req kws kwargs
                    (list (string-append "https://discordapp.com" uri))
                    #:headers (hash-set*
                               headers
                               'authorization token
                               'content-type "application/json"
                               'user-agent "DiscordBot (https://github.com/nitros12/racket-cord, 0.0)")))))

(struct http-client
  (requester
   global-limit
   ratelimits))

(define (make-http-client token)
  (http-client
   (make-discord-http token)
   (box always-evt)
   (make-hash)))

(define (discord-url . parts)
  (apply ~a "/api/v8" parts #:separator "/"))

(define gateway-params "/?v=9&encoding=json")

(struct route (method method-name path) #:transparent
  #:methods gen:custom-write
  [(define (write-proc route port _mode)
     (display (route-method-name route) port)
     (display ":" port)
     (display (route-path route) port))])

(define (make-route method method-name . path-parts)
  (route method method-name (apply discord-url path-parts)))

(define (apply-route http-client route kws kwargs)
  (let ([method (route-method route)]
        [path (route-path route)])
    (log-discord-debug "applying route: ~a ~a" route (map cons kws kwargs))
    (keyword-apply (http-client-requester http-client) kws kwargs method path '())))

(define (get-ws-url http-client)
  (let ([resp ((http-client-requester http-client)
               get
               (discord-url "gateway"))])
    (match resp 
      [(response #:status-code 200)
       (string->url (string-append (hash-ref (response-json resp) 'url) gateway-params))]
      [(response #:status-code code #:body body)
       (error (~a "http error\n"
                  "  response: " (bytes->string/utf-8 body)))])))

(define (get-ws-url-infallible http-client)
  (let retry ()
    (with-handlers ([exn:fail? (lambda (e)
                                 (sleep 5)
                                 (retry))])
      (get-ws-url http-client))))

(define (get-ws-url-bot http-client)
  (let* ([resp ((http-client-requester http-client)
                get
                (discord-url "gateway" "bot"))]
         [resp-json (response-json resp)])
    (match resp 
      [(response #:status-code 200)
       (values (string->url (string-append (hash-ref resp-json 'url) gateway-params))
               (hash-ref resp-json 'shards))]
      [(response #:status-code code #:body body)
       (error (~a "http error\n"
                  "  response: " (bytes->string/utf-8 body)))])))

(define run-route
  (make-keyword-procedure
   (lambda (kws kwargs route http-client)
     ;; Rate limits are implemented using events,
     ;; that are usually ready if and only if the route is ready
     ;; There may be exceptions, such as race conditions, so if the client still
     ;; receives a 429 error (too many requests) then the client will wait and retry.

     (define (sync-and-reset limit)
       (define evt (unbox limit))
       (sync evt)
       (unless (box-cas! limit evt always-evt)
         (sync-and-reset limit)))

     ;; wait for the global rate limit
     (define global-limit (http-client-global-limit http-client))
     (sync-and-reset global-limit)

     ;; wait for the rate limit of the current route
     (define ratelimits (http-client-ratelimits http-client))
     (define route-limit-evt (hash-ref! ratelimits route (thunk (box always-evt))))
     (sync-and-reset route-limit-evt)

     ;; make up to 5 attempts to run the route
     (let retry ([tries 5])
       (when (<= tries 0)
         (error 'run-route "Ran out of attempts on route: ~a" route))
       (define resp (apply-route http-client route kws kwargs))
       (match resp
         [(response #:status-code 502)
          (sleep (* 2 (- 6 tries)))
          (retry (sub1 tries))]
         [(response #:status-code 429)
          (define retry-after-ms
            (* 1000
               ((compose string->number bytes->string/utf-8)
                (or (response-headers-ref resp 'Retry-After) #"0"))))
          (define next-attempt-time (+ (current-inexact-milliseconds) retry-after-ms))
          (log-discord-info "Hit 429 ratelimit ~a retrying in ~a ms" route retry-after-ms)
          (let* ([globally (hash-ref (response-json resp) 'global #f)]
                 [next-evt-box (if globally global-limit route-limit-evt)])
            (set-box! next-evt-box (alarm-evt next-attempt-time))
            (sync-and-reset next-evt-box))
          (retry (sub1 tries))]
         [(response #:body body
                    #:status-code code)
          #:when (>= code 400)
          (define json (response-json resp))
          (raise (exn:fail:network:http:discord
                  (format "Discord gave us an error: ~s" body)
                  (current-continuation-marks) code
                  (hash-ref json 'code null)
                  (hash-ref json 'message null)))]
         [_
          (define remaining
            ((compose string->number bytes->string/utf-8)
             (or (response-headers-ref resp 'X-RateLimit-Remaining) #"1")))
          (when (<= remaining 0)
            (define reset-after-s
              ((compose string->number bytes->string/utf-8)
               (or (response-headers-ref resp 'X-RateLimit-Reset-After) #"0")))
            (define delta-ms (* 1000 reset-after-s))
            (define route-next-use (+ (current-inexact-milliseconds) delta-ms))
            (log-discord-info "Setting ratelimit on ~a for ~a ms" route delta-ms)
            (set-box! route-limit-evt (alarm-evt route-next-use)))
          (response-json resp)])))))

(define-syntax (define/endpoint stx)
  (define-splicing-syntax-class kwarg-pair
    [pattern (~seq kw:keyword arg:expr)])
  (define-splicing-syntax-class process-expr
    #:attributes [wrap]
    #:literals [=>]
    [pattern (~seq [=> var:id body:expr])
             #:attr wrap (lambda (stx) #`(let ([var #,stx]) body))]
    [pattern (~seq [=> func:expr])
             #:attr wrap (lambda (stx) #`(func #,stx))]
    [pattern (~seq)
             #:attr wrap (lambda (stx) stx)])
  (syntax-parse stx
    [(_ (name:id client:id . params)
        [method path ...] ;; builds the request path, can use the params
        process-expr:process-expr ;; an expression to pass the response through before returning to the caller
        body:expr ... ;; arbitrary code to run before the request
        run-args:kwarg-pair ...) ;; extra query params
     (quasisyntax/loc stx
       (begin
         (provide name)
         (define (name client . params)
           body ...
           #,((datum process-expr.wrap)
              #`(run-route (make-route method 'method path ...)
                           (client-http-client client)
                           (~@ run-args.kw run-args.arg) ...)))))]))

;; CHANNEL ENDPOINTS

(define/endpoint (get-channel _client channel-id)
  (get "channels" channel-id))

(define/endpoint (modify-channel _client channel-id data)
  (patch "channels" channel-id)
  #:data (json-payload data))

(define/endpoint (delete-channel _client channel-id)
  (delete "channels" channel-id))

(define/endpoint (get-channel-messages _client channel-id . params)
  (get "channels" channel-id "messages")
  #:params params)

(define/endpoint (get-channel-message _client channel-id message-id)
  (get "channels" channel-id "messages" message-id))

(define/endpoint (create-message _client
                                 channel-id
                                 [content ""]
                                 #:allowed-mentions [mentions #f]
                                 #:reply-to [reference #f]
                                 #:embed [embed #f]
                                 #:tts [tts #f]
                                 #:file [attachment #f])
  (post "channels" channel-id "messages")
  #:data
  (let ([data (make-hash)])
    (when embed
      (hash-set! data 'embed embed))
    (when reference
      (hash-set! data 'message_reference reference))
    (when mentions
      (hash-set! data 'allowed_mentions mentions))
    (hash-set! data 'tts tts)
    (hash-set! data 'content content)
    (if attachment
        (multipart-payload
         (file-part "file"
                    (open-input-bytes (attachment-data attachment))
                    (attachment-name attachment)
                    (attachment-type attachment))
         (field-part "payload_json" (jsexpr->string data) #"application/json"))
        (json-payload data))))

(define/endpoint (edit-message _client
                               channel-id
                               message-id
                               #:content [content null]
                               #:embed [embed null])
  (patch "channels" channel-id "messages" message-id)
  #:params (filter-null `((content . ,content) (embed . ,embed))))

(define/endpoint (delete-message _client channel-id message-id)
  (delete "channels" channel-id "messages" message-id))

(define/endpoint (create-reaction _client channel-id message-id emoji)
  (post "channels" channel-id "messages" message-id "reactions" emoji "@me"))

(define/endpoint (delete-own-reaction _client channel-id message-id emoji)
  (delete "channels" channel-id "messages" message-id "reactions" emoji "@me"))

(define/endpoint (delete-user-reaction _client channel-id message-id emoji user-id)
  (delete "channels" channel-id "messages" message-id "reactions" emoji user-id))

(define/endpoint (get-reactions _client channel-id message-id emoji . params)
  (get "channels" channel-id "messages" message-id "reactions" emoji)
  #:params params)

(define/endpoint (delete-all-reactions _client channel-id message-id)
  (delete "channels" channel-id "messages" message-id))

(define/endpoint (bulk-delete-messages _client channel-id . ids)
  (post "channels" channel-id "messages" "bulk-delete")
  #:data (json-payload (hash 'messages ids)))

(define/endpoint (edit-channel-permissions _client channel-id overwrite-id allow deny type)
  (put "channels" channel-id "permissions" overwrite-id)
  #:data (json-payload
          (hash 'allow allow
                'deny deny
                'type type)))

(define/endpoint (get-channel-invites _client channel-id)
  (get "channels" channel-id "invites"))

(define/endpoint (create-channel-invite _client
                                        channel-id
                                        [age 86400]
                                        [uses 0]
                                        [temporary #f]
                                        [unique #f])
  (post "channels" channel-id "invites")
  #:data (json-payload
          (hash 'max_age age
                'max_uses uses
                'temporary temporary
                'unique unique)))

(define/endpoint (delete-channel-permission _client channel-id overwrite-id)
  (delete "channels" channel-id "permissions" overwrite-id))

(define/endpoint (trigger-typing-indicator _client channel-id)
  (post "channels" channel-id "typing"))

(define/endpoint (get-pinned-messages _client channel-id)
  (get "channels" channel-id "pins"))

(define/endpoint (add-pinned-channel-message _client channel-id message-id)
  (put "channels" channel-id "pins" message-id))

(define/endpoint (delete-pinned-channel-message _client channel-id message-id)
  (delete "channels" channel-id "pins" message-id))

(define/endpoint (group-dm-add-recipient _client channel-id user-id access-token nick)
  (put "channels" channel-id "recipients" user-id)
  #:data (json-payload
          (hash 'access_token access-token
                'nick nick)))

(define/endpoint (group-dm-remove-recipient _client channel-id user-id)
  (delete "channels" channel-id "recipients" user-id))

;; EMOJI ENDPOINTS

(define/endpoint (list-guild-emoji _client guild-id)
  (get "guilds" guild-id "emojis"))

(define/endpoint (get-guild-emoji _client guild-id emoji-id)
  (get "guilds" guild-id "emojis" emoji-id))

(define/endpoint (create-guild-emoji _client guild-id name image image-type roles)
  (post "guilds" guild-id "emojis")
  #:data (json-payload
          (hash 'name name
                'image (image-data->base64 image-type image)
                'roles roles)))

(define/endpoint (modify-guild-emoji _client guild-id emoji-id name roles)
  (patch "guilds" guild-id "emojis" emoji-id)
  #:data (json-payload
          (hash 'name name
                'roles roles)))

(define/endpoint (delete-guild-emoji _client guild-id emoji-id)
  (delete "guilds" guild-id "emojis" emoji-id))

;; GUILD ENDPOINTS

(define/endpoint (get-guild _client guild-id)
  (get "guilds" guild-id))

(define/endpoint (modify-guild _client guild-id data)
  (patch "guilds" guild-id)
  #:data (json-payload data))

(define/endpoint (delete-guild _client guild-id)
  (delete "guilds" guild-id))

(define/endpoint (get-guild-channels _client guild-id)
  (get "guilds" guild-id "channels"))

(define/endpoint (create-guild-channel _client guild-id data)
  (post "guilds" guild-id "channnels")
  #:data (json-payload data))

(define/endpoint (modify-guild-channel-permissions _client guild-id data)
  (patch "guilds" guild-id "channels")
  #:data (json-payload data))

(define/endpoint (get-guild-member _client guild-id user-id)
  (get "guilds" guild-id "members" user-id))

(define/endpoint (list-guild-members _client guild-id #:limit [limit 1] #:after [after 0])
  (get "guilds" guild-id "members")
  #:params `((limit . ,limit) (after . ,after)))

(define/endpoint (add-guild-member _client guild-id user-id data)
  (put "guilds" guild-id "members" user-id)
  #:data (json-payload data))

(define/endpoint (modify-guild-member _client guild-id user-id data)
  (patch "guilds" guild-id "members" guild-id)
  #:data (json-payload data))

(define/endpoint (modify-user-nick _client guild-id nick)
  (patch "guilds" guild-id "members" "@me" "nick")
  #:data (json-payload (hash 'nick nick)))

(define/endpoint (add-guild-member-role _client guild-id user-id role-id)
  (put "guilds" guild-id "members" user-id "roles" role-id))

(define/endpoint (remove-guild-member-role _client guild-id user-id role-id)
  (delete "guilds" guild-id "members" user-id "roles" role-id))

(define/endpoint (remove-guild-member _client guild-id user-id)
  (delete "guilds" guild-id "members" user-id))

(define/endpoint (get-guild-bans _client guild-id)
  (get "guilds" guild-id "bans"))

(define/endpoint (create-guild-ban _client guild-id user-id [days 1])
  (put "guilds" guild-id "bans" user-id)
  #:params `((delete-message-days . ,days)))

(define/endpoint (remove-guild-ban _client guild-id user-id)
  (delete "guilds" guild-id "bans" user-id))

(define/endpoint (get-guild-roles _client guild-id)
  (get "guilds" guild-id "roles"))

(define/endpoint (create-guild-role _client guild-id data)
  (post "guilds" guild-id "roles")
  #:data (json-payload data))

(define/endpoint (modify-guild-role-positions _client guild-id data)
  (patch "guilds" guild-id "roles")
  #:data (json-payload data))

(define/endpoint (modify-guild-role _client guild-id role-id data)
  (patch "guilds" guild-id "roles" role-id)
  #:data (json-payload data))

(define/endpoint (delete-guild-role _client guild-id role-id)
  (delete "guilds" guild-id "roles" guild-id))

(define/endpoint (get-guild-prune-count _client guild-id days)
  (get "guilds" guild-id "prune")
  [=> x (hash-ref x 'pruned)]
  #:params `((days . ,days)))

(define/endpoint (begin-guild-prune _client guild-id days)
  (post "guilds" guild-id "prune")
  [=> x (hash-ref x 'pruned)]
  #:params `((days . ,days)))

;; Would be here: voice regions

(define/endpoint (get-guild-invites _client guild-id)
  (get "guilds" guild-id "invites"))

(define/endpoint (get-guild-integrations _client guild-id)
  (get "guilds" guild-id "integrations"))

(define/endpoint (create-guild-integration _client guild-id type id)
  (post "guilds" guild-id "integrations")
  #:data (json-payload (hash 'type type 'id id)))

(define/endpoint (modify-guild-integration _client guild-id integration-id data)
  (patch "guilds" guild-id "integrations" integration-id)
  #:data (json-payload data))

(define/endpoint (delete-guild-integrations _client guild-id integration-id)
  (delete "guilds" guild-id "integrations" integration-id))

(define/endpoint (sync-guild-integrations _client guild-id integration-id)
  (post "guilds" guild-id "integrations" integration-id "sync"))

(define/endpoint (get-guild-embed _client guild-id)
  (get "guilds" guild-id "embed"))

(define/endpoint (modify-guild-embed _client guild-id data)
  (patch "guilds" guild-id "embed")
  #:data (json-payload data))

;; USER ENDPOINTS

(define/endpoint (get-current-user _client)
  (get "users" "@me"))

(define/endpoint (get-user _client user-id)
  (get "users" user-id))

(define/endpoint (modify-current-user _client
                                      #:username [username null]
                                      #:avatar [avatar null]
                                      #:avatar-type [avatar-type ""])
  (patch "users" "@me")
  #:data (json-payload (hash-exclude-null
                        'username username
                        'avatar (if (null? avatar) null (image-data->base64 avatar-type avatar)))))

(define/endpoint (get-current-user-guilds client
                                          #:before [before null]
                                          #:after [after null]
                                          #:limit [limit null])
  (get "users" "@me" "guilds")
  #:params (filter-null `((before . ,before)
                          (after . ,after)
                          (limit . ,limit))))


(define/endpoint (leave-guild _client guild-id)
  (delete "users" "@me" "guilds" guild-id))

(define/endpoint (get-user-dms _client)
  (get "users" "@me" "channels"))

(define/endpoint (create-dm _client recipient-id)
  (post "users" "@me" "channels")
  #:data (json-payload (hash 'recipient_id recipient-id)))

(define/endpoint (create-group-dm _client data)
  (post "users" "@me" "channels")
  #:data (json-payload data))

;; WEBHOOK ENDPOINTS

(define/endpoint (create-webhook _client channel-id name avatar avatar-type)
  (post "channels" channel-id "webhooks")
  #:data (json-payload (hash
                        'name name
                        'avatar (image-data->base64 avatar-type avatar))))

(define/endpoint (get-channel-webhooks _client channel-id)
  (get "channels" channel-id "webhooks"))

(define/endpoint (get-guild-webhooks _client guild-id)
  (get "guilds" guild-id "webhooks"))

(define/endpoint (get-webhook _client webhook-id)
  (get "webhooks" webhook-id))

(define/endpoint (get-webhook-with-token _client webhook-id webhook-token)
  (get "webhooks" webhook-id webhook-token))

(define/endpoint (modify-webhook _client
                                 webhook-id
                                 #:name [name null]
                                 #:avatar [avatar null]
                                 #:avatar-type [avatar-type ""]
                                 #:channel-id [channel-id null])
  (patch "webhooks" webhook-id)
  #:data (json-payload (hash-exclude-null
                        'name name
                        'avatar (if (null? avatar) null (image-data->base64 avatar-type avatar))
                        'channel_id channel-id)))

(define/endpoint (modify-webhook-with-token _client
                                            webhook-id
                                            token
                                            #:name [name null]
                                            #:avatar [avatar null]
                                            #:avatar-type [avatar-type ""]
                                            #:channel-id [channel-id null])
  (patch "webhooks" webhook-id)
  #:data (json-payload (hash-exclude-null
                        'name name
                        'avatar (if (null? avatar) null (image-data->base64 avatar-type avatar))
                        'channel_id channel-id)))

(define/endpoint (delete-webhook _client webhook-id)
  (delete "webhooks" webhook-id))

(define/endpoint (delete-webhook-with-token _client webhook-id webhook-token)
  (delete "webhooks" webhook-id webhook-token))

(define/endpoint (execute-webhook _client webhook-id webhook-token data #:wait [wait #f])
  (post "webhooks" webhook-id webhook-token)
  #:params `((wait . ,wait))
  #:data (json-payload data))
