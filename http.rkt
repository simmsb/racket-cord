#lang racket

(require net/http-easy
         json
         net/url
         racket/string
         srfi/19
         "data.rkt"
         "utils.rkt")

(provide make-http-client
         get-ws-url
         get-ws-url-bot
         (struct-out exn:fail:network:http:discord)
         (contract-out
          [attachment
           (-> bytes? (or/c string? bytes?) (or/c string? bytes?) attachment?)]))

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
   global-lock
   ratelimits))

(define (make-http-client token)
  (http-client
   (make-discord-http token)
   (make-semaphore 1)
   (make-hash)))

(define (format-route method path channel-id guild-id webhook-id)
  (format "~a|~a|~a|~a|~a" method path channel-id guild-id webhook-id))

(struct route
  (method
   path
   bucket-key
   channel-id
   guild-id
   webhook-id)
  #:transparent)

(define (make-route method
                    #:channel-id [channel-id null]
                    #:guild-id [guild-id null]
                    #:webhook-id [webhook-id null] . path-parts)
  (route
   method
   (apply discord-url path-parts)
   (format-route method (string-join path-parts) channel-id guild-id webhook-id)
   channel-id guild-id webhook-id))

(define (apply-route http-client route args kws kwargs)
  (let ([method (route-method route)]
        [formatted (foldl (lambda (k s)
                            (string-replace s (format "{~a}" (car k)) (~a (cdr k))))
                          (route-path route)
                          (append (list (cons "channel-id" (route-channel-id route))
                                        (cons "guild-id"   (route-guild-id route))
                                        (cons "webhook-id" (route-webhook-id route)))
                                  args))])
    (log-discord-debug "applying route: ~a. ~a" formatted (map cons kws kwargs))
    (keyword-apply (http-client-requester http-client) kws kwargs method formatted '())))

(define gateway-params "/?v=8&encoding=json")

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

(define (discord-url . parts)
  (format "/api/v8/~a" (string-join parts "/")))

(define (rfc2822->unix-seconds s)
  (time-second (date->time-utc (string->date s "~a, ~d ~b ~Y ~H:~M:~S"))))

(define run-route
  (make-keyword-procedure
   (lambda (kws kwargs route http-client [args null])
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
             (when (<= tries 0)
               (error 'run-route "Ran out of attempts on route: ~a" route-key))
             (let ([resp (apply-route http-client route args kws kwargs)])
               (match resp
                 [(response #:status-code 502)
                  (sleep (* 2 (- 6 tries)))
                  (retry (sub1 tries))]
                 [(response #:body body
                            #:headers ([Retry-After retry])
                            #:status-code 429)
                  (let ([retry-after (/ (string->number retry) 1000)]
                        [globally (hash-ref (response-json resp) 'global #f)])
                    (log-discord-info "Hit 429 ratelimit ~a retrying in ~a seconds" route-key retry-after)
                    (if globally
                        (call-with-semaphore global-lock (thunk (sleep retry-after)))
                        (sleep retry-after)))]
                 [(response #:body body
                            #:status-code code)
                  #:when (>= code 400)
                  (let ([json (response-json resp)])
                    (raise (exn:fail:network:http:discord
                            (format "Discord gave us an error: ~a" body)
                            (current-continuation-marks) code
                            (hash-ref json 'code null)
                            (hash-ref json 'message null))))]
                 [_
                  (let* ([remaining ((compose string->number bytes->string/utf-8)
                                     (or (response-headers-ref resp 'X-Ratelimit-Remaining) #"0"))]
                         [reset ((compose string->number bytes->string/utf-8)
                                 (or (response-headers-ref resp 'X-Ratelimit-Reset) #"0"))]
                         [date ((compose rfc2822->unix-seconds bytes->string/utf-8)
                                (response-headers-ref resp 'Date))])
                    (when (<= remaining 0)
                      (let ([delta (max 0 (- reset date))])
                        (log-discord-info "Sleeping on ratelimit ~a for ~a seconds" route-key delta)
                        (sleep delta)))
                    (response-json resp))]))))))))))

;; CHANNEL ENDPOINTS

(provide get-channel modify-channel delete-channel
         get-channel-messages get-channel-message
         create-message edit-message delete-message bulk-delete-messages
         create-reaction delete-own-reaction delete-user-reaction get-reactions delete-all-reactions
         edit-channel-permissions delete-channel-permission
         get-channel-invites create-channel-invite
         trigger-typing-indicator
         get-pinned-messages add-pinned-channel-message delete-pinned-channel-message
         group-dm-add-recipient group-dm-remove-recipient)

(define (get-channel client channel-id)
  (run-route (make-route get "channels" "{channel-id}"
                         #:channel-id channel-id)
             (client-http-client client)))

(define (modify-channel client channel-id data)
  (run-route (make-route patch "channels" "{channel-id}"
                         #:channel-id channel-id)
             (client-http-client client) #:data (json-payload data)))

(define (delete-channel client channel-id)
  (run-route (make-route delete "channels" "{channel-id}"
                         #:channel-id channel-id)
             (client-http-client client)))

(define (get-channel-messages client channel-id . params)
  (run-route (make-route get "channels" "{channel-id}" "messages"
                         #:channel-id channel-id)
             (client-http-client client) #:params params))

(define (get-channel-message client channel-id message-id)
  (run-route
   (make-route get "channels" "{channel-id}" "messages" "{message-id}"
               #:channel-id channel-id)
   (client-http-client client) `((message-id . ,message-id))))

(define (create-message client channel-id
                        [content ""]
                        #:allowed-mentions [mentions #f]
                        #:reply-to [reference #f]
                        #:embed [embed #f]
                        #:tts [tts #f]
                        #:file [attachment #f])
  (let ([data (make-hash)])
    (when embed
      (hash-set! data 'embed embed))
    (when reference
      (hash-set! data 'message_reference reference))
    (when mentions
      (hash-set! data 'allowed_mentions mentions))
    (hash-set! data 'tts tts)
    (hash-set! data 'content content)
    (run-route (make-route post "channels" "{channel-id}" "messages" #:channel-id channel-id)
               (client-http-client client)
               #:data
               (if attachment
                   (multipart-payload
                    (file-part "file"
                               (open-input-bytes (attachment-data attachment))
                               (attachment-name attachment)
                               (attachment-type attachment))
                    (field-part "payload_json" (jsexpr->string data) #"application/json"))
                   (json-payload data)))))

(define (edit-message client channel-id message-id #:content [content null] #:embed [embed null])
  (run-route (make-route patch "channels" "{channel-id}" "messages" "{message-id}"
                         #:channel-id channel-id)
             (client-http-client client) `((message-id . ,message-id))
             #:params (filter-null `((content . ,content)
                                     (embed . ,embed)))))

(define (delete-message client channel-id message-id)
  (run-route (make-route delete "channels" "{channel-id}" "messages" "{message-id}"
                         #:channel-id channel-id)
             (client-http-client client) `((message-id . ,message-id))))

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

(define (get-reactions client channel-id message-id emoji . params)
  (run-route (make-route get "channels" "{channel-id}" "messages" "{message-id}" "reactions" "{emoji}"
                         #:channel-id channel-id)
             (client-http-client client) `((message-id . ,message-id) (emoji . ,emoji))
             #:params params))

(define (delete-all-reactions client channel-id message-id)
  (run-route (make-route delete "channels" "{channel-id}" "messages" "{message-id}"
                         #:channel-id channel-id)
             (client-http-client client) `((message-id . ,message-id))))

(define (bulk-delete-messages client channel-id . ids)
  (run-route (make-route post "channels" "{channel-id}" "messages" "bulk-delete"
                         #:channel-id channel-id)
             (client-http-client client) #:data (json-payload (hash 'messages ids))))

(define (edit-channel-permissions client channel-id overwrite-id allow deny type)
  (run-route (make-route put "channels" "{channel-id}" "permissions" "{overwrite-id}"
                         #:channel-id channel-id)
             (client-http-client client) `((overwrite-id . ,overwrite-id))
             #:data (json-payload
                     (hash 'allow allow
                           'deny deny
                           'type type))))

(define (get-channel-invites client channel-id)
  (run-route (make-route get "channels" "{channel-id}" "invites"
                         #:channel-id channel-id)
             (client-http-client client)))

(define (create-channel-invite client channel-id [age 86400] [uses 0] [temporary #f] [unique #f])
  (run-route (make-route post "channels" "{channel-id}" "invites"
                         #:channel-id channel-id)
             (client-http-client client) #:data (json-payload
                                                 (hash 'max_age age
                                                       'max_uses uses
                                                       'temporary temporary
                                                       'unique unique))))

(define (delete-channel-permission client channel-id overwrite-id)
  (run-route (make-route delete "channels" "{channel-id}" "permissions" "{overwrite-id}"
                         #:channel-id channel-id)
             (client-http-client client) `((overwrite-id . ,overwrite-id))))

(define (trigger-typing-indicator client channel-id)
  (run-route (make-route post "channels" "{channel-id}" "typing"
                         #:channel-id channel-id)
             (client-http-client client)))

(define (get-pinned-messages client channel-id)
  (run-route (make-route get "channels" "{channel-id}" "pins"
                         #:channel-id channel-id)
             (client-http-client client)))

(define (add-pinned-channel-message client channel-id message-id)
  (run-route (make-route put "channels" "{channel-id}" "pins" "{message-id}"
                         #:channel-id channel-id)
             (client-http-client client) `((message-id . ,message-id))))

(define (delete-pinned-channel-message client channel-id message-id)
  (run-route (make-route delete "channels" "{channel-id}" "pins" "{message-id}"
                         #:channel-id channel-id)
             (client-http-client client) `((message-id . ,message-id))))

(define (group-dm-add-recipient client channel-id user-id access-token nick)
  (run-route (make-route put "channels" "{channel-id}" "recipients" "{user-id}"
                         #:channel-id channel-id)
             (client-http-client client) `((user-id . ,user-id))
             #:data (json-payload
                     (hash 'access_token access-token
                           'nick nick))))

(define (group-dm-remove-recipient client channel-id user-id)
  (run-route (make-route delete "channels" "{channel-id}" "recipients" "{user-id}"
                         #:channel-id channel-id)
             (client-http-client client) `((channel-id . ,channel-id))))


;; EMOJI ENDPOINTS

(provide list-guild-emoji get-guild-emoji create-guild-emoji
         modify-guild-emoji delete-guild-emoji)

(define (list-guild-emoji client guild-id)
  (run-route (make-route get "guilds" "{guild-id}" "emojis"
                         #:guild-id guild-id)
             (client-http-client client)))

(define (get-guild-emoji client guild-id emoji-id)
  (run-route (make-route get "guilds" "{guild-id}" "emojis" "{emoji-id}"
                         #:guild-id guild-id)
             (client-http-client client) `((emoji-id . ,emoji-id))))


(define (create-guild-emoji client guild-id name image image-type roles)
  (run-route (make-route post "guilds" "{guild-id}" "emojis"
                         #:guild-id guild-id)
             (client-http-client client) #:data (json-payload
                                                 (hash 'name name
                                                       'image (image-data->base64 image-type image)
                                                       'roles roles))))

(define (modify-guild-emoji client guild-id emoji-id name roles)
  (run-route (make-route patch "guilds" "{guild-id}" "emojis" "{emoji-id}"
                         #:guild-id guild-id)
             (client-http-client client) `((emoji-id . ,emoji-id))
             #:data (json-payload
                     (hash 'name name
                           'roles roles))))

(define (delete-guild-emoji client guild-id emoji-id)
  (run-route (make-route delete "guilds" "{guild-id}" "emojis" "{emoji-id}"
                         #:guild-id guild-id)
             (client-http-client client) `((emoji-id . ,emoji-id))))


;; GUILD ENDPOINTS

(provide get-guild modify-guild delete-guild
         get-guild-channels create-guild-channel modify-guild-channel-permissions
         get-guild-member list-guild-members add-guild-member modify-guild-member remove-guild-member
         modify-user-nick
         add-guild-member-role remove-guild-member-role
         get-guild-bans create-guild-ban remove-guild-ban
         get-guild-roles create-guild-role modify-guild-role modify-guild-role-positions delete-guild-role
         get-guild-prune-count begin-guild-prune
         get-guild-invites
         get-guild-integrations create-guild-integration modify-guild-integration delete-guild-integrations sync-guild-integrations
         get-guild-embed modify-guild-embed)

(define (get-guild client guild-id)
  (run-route (make-route get "guilds" "{guild-id}"
                         #:guild-id guild-id)
             (client-http-client client)))

(define (modify-guild client guild-id data)
  (run-route (make-route patch "guilds" "{guild-id}"
                         #:guild-id guild-id)
             (client-http-client client) #:data (json-payload data)))

(define (delete-guild client guild-id)
  (run-route (make-route delete "guilds" "{guild-id}"
                         #:guild-id guild-id)
             (client-http-client client)))

(define (get-guild-channels client guild-id)
  (run-route (make-route get "guilds" "{guild-id}" "channels"
                         #:guild-id guild-id)
             (client-http-client client)))

(define (create-guild-channel client guild-id data)
  (run-route
   (make-route post "guilds" "{guild-id}" "channnels"
               #:guild-id guild-id)
   (client-http-client client) #:data (json-payload data)))

(define (modify-guild-channel-permissions client guild-id data)
  (run-route (make-route patch "guilds" "{guild-id}" "channels"
                         #:guild-id guild-id)
             (client-http-client client) #:data (json-payload data)))

(define (get-guild-member client guild-id user-id)
  (run-route
   (make-route get "guilds" "{guild-id}" "members" "{user-id}"
               #:guild-id guild-id)
   (client-http-client client) `((user-id . ,user-id))))

(define (list-guild-members client guild-id #:limit [limit 1] #:after [after 0])
  (run-route (make-route get "guilds" "{guild-id}" "members"
                         #:guild-id guild-id)
             (client-http-client client)
             #:params `((limit . ,limit) (after . ,after))))

(define (add-guild-member client guild-id user-id data)
  (run-route
   (make-route put "guilds" "{guild-id}" "members" "{user-id}"
               #:guild-id guild-id)
   (client-http-client client) `((user-id . ,user-id))
   #:data (json-payload data)))

(define (modify-guild-member client guild-id user-id data)
  (run-route (make-route patch "guilds" "{guild-id}" "members" "{guild-id}"
                         #:guild-id guild-id)
             (client-http-client client) `((user-id . ,user-id))
             #:data (json-payload data)))

(define (modify-user-nick client guild-id nick)
  (run-route (make-route patch "guilds" "{guild-id}" "members" "@me" "nick"
                         #:guild-id guild-id)
             (client-http-client client) #:data (json-payload (hash 'nick nick))))

(define (add-guild-member-role client guild-id user-id role-id)
  (run-route (make-route put "guilds" "{guild-id}" "members" "{user-id}" "roles" "{role-id}"
                         #:guild-id guild-id)
             (client-http-client client) `((user-id . ,user-id) (role-id . ,role-id))))

(define (remove-guild-member-role client guild-id user-id role-id)
  (run-route (make-route delete "guilds" "{guild-id}" "members" "{user-id}" "roles" "{role-id}"
                         #:guild-id guild-id)
             (client-http-client client) `((user-id . ,user-id) (role-id . ,role-id))))

(define (remove-guild-member client guild-id user-id)
  (run-route (make-route delete "guilds" "{guild-id}" "members" "{user-id}"
                         #:guild-id guild-id)
             (client-http-client client) `((user-id . ,user-id))))

(define (get-guild-bans client guild-id)
  (run-route (make-route get "guilds" "{guild-id}" "bans"
                         #:guild-id guild-id)
             (client-http-client client)))

(define (create-guild-ban client guild-id user-id [days 1])
  (run-route (make-route put "guilds" "{guild-id}" "bans" "{user-id}"
                         #:guild-id guild-id)
             (client-http-client client) `((user-id . ,user-id))
             #:params `((delete-message-days . ,days))))

(define (remove-guild-ban client guild-id user-id)
  (run-route (make-route delete "guilds" "{guild-id}" "bans" "{user-id}"
                         #:guild-id guild-id)
             (client-http-client client) `((user-id . ,user-id))))

(define (get-guild-roles client guild-id)
  (run-route (make-route get "guilds" "{guild-id}" "roles"
                         #:guild-id guild-id)
             (client-http-client client)))

(define (create-guild-role client guild-id data)
  (run-route (make-route post "guilds" "{guild-id}" "roles"
                         #:guild-id guild-id)
             (client-http-client client) #:data (json-payload data)))

(define (modify-guild-role-positions client guild-id data)
  (run-route (make-route patch "guilds" "{guild-id}" "roles"
                         #:guild-id guild-id)
             (client-http-client client) #:data (json-payload data)))

(define (modify-guild-role client guild-id role-id data)
  (run-route
   (make-route patch "guilds" "{guild-id}" "roles" "{role-id}"
               #:guild-id guild-id)
   (client-http-client client) `((role-id . ,role-id))
   #:data (json-payload data)))

(define (delete-guild-role client guild-id role-id)
  (run-route (make-route delete "guilds" "{guild-id}" "roles" "{guild-id}"
                         #:guild-id guild-id)
             (client-http-client client) `((role-id . ,role-id))))

(define (get-guild-prune-count client guild-id days)
  (hash-ref (run-route (make-route get "guilds" "{guild-id}" "prune"
                                   #:guild-id guild-id)
                       (client-http-client client)
                       #:params `((days . ,days)))
            'pruned))

(define (begin-guild-prune client guild-id days)
  (hash-ref (run-route (make-route post "guilds" "{guild-id}" "prune"
                                   #:guild-id guild-id)
                       (client-http-client client)
                       #:params `((days . ,days)))
            'pruned))

;; Would be here: voice regions

(define (get-guild-invites client guild-id)
  (run-route (make-route get "guilds" "{guild-id}" "invites"
                         #:guild-id guild-id)
             (client-http-client client)))

(define (get-guild-integrations client guild-id)
  (run-route (make-route get "guilds" "{guild-id}" "integrations"
                         #:guild-id guild-id)
             (client-http-client client)))

(define (create-guild-integration client guild-id type id)
  (run-route (make-route post "guilds" "{guild-id}" "integrations"
                         #:guild-id guild-id)
             (client-http-client client) #:data (json-payload
                                                 (hash 'type type 'id id))))

(define (modify-guild-integration client guild-id integration-id data)
  (run-route (make-route patch "guilds" "{guild-id}" "integrations" "{integration-id}"
                         #:guild-id guild-id)
             (client-http-client client) `((integration-id . ,integration-id))
             #:data (json-payload data)))

(define (delete-guild-integrations client guild-id integration-id)
  (run-route (make-route delete "guilds" "{guild-id}" "integrations" "{integration-id}"
                         #:guild-id guild-id)
             (client-http-client client) `((integration-id . ,integration-id))))

(define (sync-guild-integrations client guild-id integration-id)
  (run-route (make-route post "guilds" "{guild-id}" "integrations" "{integration-id}" "sync"
                         #:guild-id guild-id)
             (client-http-client client) `((integration-id . ,integration-id))))

(define (get-guild-embed client guild-id)
  (run-route (make-route get "guilds" "{guild-id}" "embed"
                         #:guild-id guild-id)
             (client-http-client client)))

(define (modify-guild-embed client guild-id data)
  (run-route (make-route patch "guilds" "{guild-id}" "embed"
                         #:guild-id guild-id)
             (client-http-client client) #:data (json-payload data)))

;; USER ENDPOINTS

(provide get-current-user get-user modify-current-user
         get-current-user-guilds leave-guild
         get-user-dms create-dm create-group-dm)

(define (get-current-user client)
  (run-route (make-route get "users" "@me")
             (client-http-client client)))

(define (get-user client user-id)
  (run-route (make-route get "users" "{user-id}")
             (client-http-client client) `((user-id . ,user-id))))

(define (modify-current-user client #:username [username null] #:avatar [avatar null] #:avatar-type [avatar-type ""])
  (run-route (make-route patch "users" "@me")
             (client-http-client client)
             #:data (json-payload (hash-exclude-null
                                   'username username
                                   'avatar (if (null? avatar) null (image-data->base64 avatar-type avatar))))))

(define (get-current-user-guilds client
                                 #:before [before null]
                                 #:after [after null]
                                 #:limit [limit null])
  (run-route (make-route get "users" "@me" "guilds")
             (client-http-client client)
             #:params (filter-null `((before . ,before)
                                     (after . ,after)
                                     (limit . ,limit)))))


(define (leave-guild client guild-id)
  (run-route (make-route delete "users" "@me" "guilds" "{guild-id}"
                         #:guild-id guild-id)
             (client-http-client client)))

(define (get-user-dms client)
  (run-route (make-route get "users" "@me" "channels")
             (client-http-client client)))

(define (create-dm client recipient-id)
  (run-route
   (make-route post "users" "@me" "channels")
   (client-http-client client)
   #:data (json-payload (hash 'recipient_id recipient-id))))

(define (create-group-dm client data)
  (run-route
   (make-route post "users" "@me" "channels")
   (client-http-client client)
   #:data (json-payload data)))

;; WEBHOOK ENDPOINTS
(provide create-webhook
         get-channel-webhooks get-guild-webhooks
         get-webhook get-webhook-with-token
         modify-webhook modify-webhook-with-token
         delete-webhook delete-webhook-with-token
         execute-webhook)

(define (create-webhook client channel-id name avatar avatar-type)
  (run-route
   (make-route post "channels" "{channel-id}" "webhooks"
               #:channel-id channel-id)
   (client-http-client client)
   #:data (json-payload (hash
                         'name name
                         'avatar (image-data->base64 avatar-type avatar)))))

(define (get-channel-webhooks client channel-id)
  (run-route (make-route get "channels" "{channel-id}" "webhooks"
                         #:channel-id channel-id)
             (client-http-client client)))

(define (get-guild-webhooks client guild-id)
  (run-route (make-route get "guilds" "{guild-id}" "webhooks"
                         #:guild-id guild-id)
             (client-http-client client)))

(define (get-webhook client webhook-id)
  (run-route
   (make-route get "webhooks" "{webhook-id}"
               #:webhook-id webhook-id)
   (client-http-client client)))

(define (get-webhook-with-token client webhook-id webhook-token)
  (run-route
   (make-route get "webhooks" "{webhook-id}" "{webhook-token}"
               #:webhook-id webhook-id)
   (client-http-client client) `((webhook-token . ,webhook-token))))

(define (modify-webhook client webhook-id
                        #:name [name null]
                        #:avatar [avatar null]
                        #:avatar-type [avatar-type ""]
                        #:channel-id [channel-id null])
  (run-route
   (make-route patch "webhooks" "{webhook-id}"
               #:webhook-id webhook-id)
   (client-http-client client)
   #:data (json-payload (hash-exclude-null
                         'name name
                         'avatar (if (null? avatar) null (image-data->base64 avatar-type avatar))
                         'channel_id channel-id))))

(define (modify-webhook-with-token client webhook-id token
                                   #:name [name null]
                                   #:avatar [avatar null]
                                   #:avatar-type [avatar-type ""]
                                   #:channel-id [channel-id null])
  (run-route
   (make-route patch "webhooks" "{webhook-id}"
               #:webhook-id webhook-id)
   (client-http-client client)
   #:data (json-payload (hash-exclude-null
                         'name name
                         'avatar (if (null? avatar) null (image-data->base64 avatar-type avatar))
                         'channel_id channel-id))))

(define (delete-webhook client webhook-id)
  (run-route (make-route delete "webhooks" "{webhook-id}"
                         #:webhook-id webhook-id)
             (client-http-client client)))

(define (delete-webhook-with-token client webhook-id webhook-token)
  (run-route (make-route delete "webhooks" "{webhook-id}" "{webhook-token}"
                         #:webhook-id webhook-id)
             (client-http-client client) `((webhook-token . ,webhook-token))))

(define (execute-webhook client webhook-id webhook-token data #:wait [wait #f])
  (run-route (make-route post "webhooks" "{webhook-id}" "{webhook-token}"
                         #:webhook-id webhook-id)
             (client-http-client client)
             #:params `((wait . ,wait))
             #:data (json-payload data)))
