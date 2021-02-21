#lang racket

(require racket/hash
         "utils.rkt")

(provide (struct-out client)
         (struct-out ws-client)
         (struct-out guild)
         (struct-out guild-channel)
         (struct-out dm-channel)
         (struct-out user)
         (struct-out member)
         (struct-out message)
         (struct-out role)
         (struct-out emoji)
         (struct-out game)
         (struct-out invite)
         (struct-out webhook)
         hash->guild
         hash->channel
         hash->user
         hash->member
         hash->message
         hash->role
         hash->emoji
         hash->game
         hash->invite
         hash->webhook
         game->hash
         update-guild
         update-channel
         update-user
         update-member)

(struct client
  (shards
   shard-threads
   user
   guilds
   private-channels
   events
   http-client
   token
   intents
   running)
  #:mutable
  #:transparent)

(struct ws-client
  (token
   client
   shard-id
   
   [gateway-url #:mutable]
   [socket #:mutable]
   [heartbeat-thread #:mutable]
   [recv-thread #:mutable]
   [heartbeat-acked #:mutable]
   [stop-channel #:mutable]

   [session-id #:mutable]
   [seq #:mutable]))

(struct/contract user
  ([id (or/c string? #f)]
   [username string?]
   [discriminator string?]
   [avatar (or/c string? #f)]
   [bot boolean?]
   [system boolean?]
   [mfa-enabled boolean?]
   [locale (or/c string? #f)]
   [verified boolean?]
   [email (or/c string? #f)]
   [flags (or/c integer? #f)]
   [premium-type (or/c integer? #f)]
   [public-flags (or/c integer? #f)])
  #:transparent)

(define (hash->user data)
  (and data
       (user
        (hash-ref data 'id)
        (hash-ref data 'username)
        (hash-ref data 'discriminator)
        (hash-ref data 'avatar #f)
        (hash-ref data 'bot #f)
        (hash-ref data 'system #f)
        (hash-ref data 'mfa_enabled #f)
        (hash-ref data 'locale #f)
        (hash-ref data 'verified #f)
        (hash-ref data 'email #f)
        (hash-ref data 'flags #f)
        (hash-ref data 'premium_type #f)
        (hash-ref data 'public_flags #f))))

(struct/contract emoji
  ([id (or/c string? #f)]
   [name (or/c string? #f)]
   [roles (listof string?)]
   [user (or/c user? #f)]
   [require-colons boolean?]
   [managed boolean?]
   [animated boolean?]
   [available boolean?])
  #:transparent)

(define (hash->emoji data)
  (and data
       (emoji
        (hash-ref data 'id #f)
        (hash-ref data 'name #f)
        (hash-ref data 'roles '())
        (hash->user (hash-ref data 'user #f))
        (hash-ref data 'require_colons #f)
        (hash-ref data 'managed #f)
        (hash-ref data 'animated #f)
        (hash-ref data 'available #f))))

(struct/contract
 reaction
 ([count integer?]
  [me boolean?]
  [emoji emoji?])
 #:transparent)

(define (hash->reaction data)
  (and data
       (reaction
        (hash-ref data 'count)
        (hash-ref data 'me)
        (hash->emoji (hash-ref data 'emoji)))))

(struct guild
  (shard-id
   id
   name
   icon
   splash
   owner-id
   region
   afk-channel-id
   afk-timeout
   embed-enabled
   embed-channel-id
   verification-level
   default-message-notifications
   explicit-content-filter
   roles
   [emojis #:mutable]
   features
   mfa-level
   application-id
   widget-enabled
   widget-channel-id
   joined-at
   large
   unavailable
   member-count
   voice-states
   [members #:mutable]
   channels
   presences)
  #:transparent)

(struct guild-channel
  (id
   type
   guild-id
   position
   permission-overwrites
   name
   topic
   nsfw
   last-message-id
   bitrate
   user-limit
   parent-id)
  #:transparent)

(struct dm-channel
  (id
   type
   name
   last-message-id
   icon
   recipients
   owner-id
   application-id)
  #:transparent)

(struct/contract member
  ([user (or/c user? #f)]
   [nick (or/c string? #f)]
   [roles (listof string?)]
   [joined-at string?]
   [premium-since (or/c string? #f)]
   [deaf boolean?]
   [mute boolean?]
   [pending boolean?]
   [permissions (or/c string? #f)])
  #:transparent) 

(define (hash->member data)
  (and data
       (member
        (let ([ud (hash-ref data 'user #f)])
          (and ud
               (not (hash-empty? ud)) ; Apparently this optional, nonnull field can also just be an empty object. WTF???
               (hash->user ud)))
        (hash-ref data 'nick #f)
        (hash-ref data 'roles)
        (hash-ref data 'joined_at)
        (hash-ref data 'premium_since #f)
        (hash-ref data 'deaf)
        (hash-ref data 'mute)
        (hash-ref data 'pending #f)
        (hash-ref data 'permissions #f))))

(struct/contract
 attachment
 ([id string?]
  [filename string?]
  [size integer?]
  [url string?]
  [proxy-url string?]
  [height (or/c integer? #f)]
  [width (or/c integer? #f)])
 #:transparent)

(define (hash->attachment data)
  (and data
       (attachment
        (hash-ref data 'id)
        (hash-ref data 'filename)
        (hash-ref data 'size)
        (hash-ref data 'url)
        (hash-ref data 'proxy-url)
        (hash-ref data 'height #f)
        (hash-ref data 'width #f))))

(struct/contract
 embed
 ([title (or/c string? #f)]
  [type (or/c string? #f)]
  [description (or/c string? #f)]
  [url (or/c string? #f)]
  [timestamp (or/c string? #f)]
  [color (or/c integer? #f)]
  ; XXX untyped
  [footer any/c]
  [image any/c]
  [thumbnail any/c]
  [video any/c]
  [provider any/c]
  [author any/c]
  [fields any/c])
 #:transparent)

(define (hash->embed data)
  (and data
       (embed
        (hash-ref data 'title #f)
        (hash-ref data 'type #f)
        (hash-ref data 'description #f)
        (hash-ref data 'url #f)
        (hash-ref data 'timestamp #f)
        (hash-ref data 'color #f)
        (hash-ref data 'footer #f)
        (hash-ref data 'image #f)
        (hash-ref data 'thumbnail #f)
        (hash-ref data 'video #f)
        (hash-ref data 'provider #f)
        (hash-ref data 'author #f)
        (hash-ref data 'fields #f))))

(struct/contract
 message-reference
 ([message-id (or/c string? #f)]
  [channel-id (or/c string? #f)]
  [guild-id (or/c string? #f)]
  [fail-if-not-exists boolean?])
 #:transparent)

(define (hash->message-reference data)
  (and data
       (message-reference 
        (hash-ref data 'message_id #f)
        (hash-ref data 'channel_id #f)
        (hash-ref data 'guild_id #f)
        (hash-ref data 'fail_if_not_exists #f))))

(struct/contract message
  ([id string?]
   [channel-id string?]
   [guild-id (or/c string? #f)]
   [author user?]
   [member (or/c member? #f)]
   [content string?]
   [timestamp string?]
   [edited-timestamp (or/c string? #f)]
   [tts boolean?]
   [mention-everyone boolean?]
   [mentions (listof user?)] ; XXX: "Additional member field"
   [mention-roles (listof string?)]
   [mention-channels (listof any/c)]
   [attachments (listof attachment?)]
   [embeds (listof embed?)]
   [reactions (listof reaction?)]
   [nonce (or/c integer? string? #f)]
   [pinned boolean?]
   [webhook_id (or/c string? #f)]
   [type integer?]
   [activity any/c]
   [application any/c]
   [message-reference (or/c message-reference? #f)]
   [flags (or/c integer? #f)]
   [stickers (listof any/c)]
   [referenced-message (or/c (recursive-contract message? #:flat) #f)])
  #:transparent)

(define (hash->message data)
  (and data 
       (message
        (hash-ref data 'id)
        (hash-ref data 'channel_id)
        (hash-ref data 'guild_id #f)
        (hash->user (hash-ref data 'author))
        (hash->member (hash-ref data 'member #f))
        (hash-ref data 'content)
        (hash-ref data 'timestamp) ;; TODO: parse timestamps
        (hash-ref data 'edited_timestamp #f)
        (hash-ref data 'tts)
        (hash-ref data 'mention_everyone)
        (map hash->user (hash-ref data 'mentions))
        (hash-ref data 'mention_roles)
        (hash-ref data 'mention_channels null)
        (map hash->attachment (hash-ref data 'attachments))
        (map hash->embed (hash-ref data 'embeds))
        (map hash->reaction (hash-ref data 'reactions null))
        (hash-ref data 'nonce #f)
        (hash-ref data 'pinned)
        (hash-ref data 'webhook_id #f)
        (hash-ref data 'type)
        (hash-ref data 'activity #f)
        (hash-ref data 'application #f)
        (hash->message-reference (hash-ref data 'message-reference #f))
        (hash-ref data 'flags #f)
        (hash-ref data 'stickers null)
        (hash->message (hash-ref data 'referenced_message #f)))))

(struct/contract discord-role-tags
  ([bot-id (or/c string? #f)]
   [integration-id (or/c string? #f)]
   ; XXX premium_subscriber typed as "null" in docs,
   ; typing as nonnull boolean for now (true if presence, false if absent)
   [premium-subscriber boolean?]) 
  #:transparent)

(define (hash->role-tags data)
  (and data
       (discord-role-tags
        (hash-ref data 'bot_id #f)
        (hash-ref data 'integration_id #f)
        (hash-ref data 'premium_subscriber #f))))

(struct/contract role
  ([id string?]
   [name string?]
   [color integer?]
   [hoist boolean?]
   [position integer?]
   [permissions string?]
   [managed boolean?]
   [mentionable boolean?]
   [tags (or/c discord-role-tags? #f)])
  #:transparent)

(define (hash->role data)
  (and data
       (role
        (hash-ref data 'id)
        (hash-ref data 'name)
        (hash-ref data 'color)
        (hash-ref data 'hoist)
        (hash-ref data 'position)
        (hash-ref data 'permissions)
        (hash-ref data 'managed)
        (hash-ref data 'mentionable)
        (hash->role-tags (hash-ref data 'tags #f)))))

(struct game
  (name
   type
   url)
  #:transparent)

(struct/contract invite
  ([code string?]
   [guild (or/c guild? #f)]
   [channel any/c] ; TODO dispatch between guild/dm channel
   [inviter (or/c user? #f)]
   [target-user (or/c user? #f)]
   [target-user-type (or/c integer? #f)]
   [approximate-presence-count (or/c integer? #f)]
   [approximate-member-count (or/c integer? #f)])
  #:transparent)

(define (hash->invite data)
  (and data
       (invite
        (hash-ref data 'code)
        #f ;TODO fix guild(hash->guild (hash-ref data 'guild #f))
        (hash-ref data 'channel)
        (hash->user (hash-ref data 'inviter #f))
        (hash->user (hash-ref data 'target_user #f))
        (hash-ref data 'target_user_type #f)
        (hash-ref data 'approximate_presence_count #f)
        (hash-ref data 'approximate_member_count #f))))

(struct/contract
 webhook
 ([id string?]
  [type integer?]
  [guild-id (or/c string? #f)]
  [channel-id string?]
  [user (or/c user? #f)]
  [name (or/c string? #f)]
  [avatar (or/c string? #f)]
  [token (or/c string? #f)]
  [application-id (or/c string? #f)])
  #:transparent)

(define (hash->webhook data)
  (and data
  (webhook
   (hash-ref data 'id)
   (hash-ref data 'type)
   (hash-ref data 'guild_id #f)
   (hash-ref data 'channel_id)
   (hash->user (hash-ref data 'user #f))
   (hash-ref data 'name #f)
   (hash-ref data 'avatar #f)
   (hash-ref data 'token #f)
   (hash-ref data 'application_id #f))))

(define ((get-id parser) data)
  (cons (hash-ref data 'id) (parser data)))

(define (extract-and-parse data value parser)
  (bindmap parser (hash-ref data value null)))

(define (hash->guild shard-id data)
  (guild
   shard-id
   (hash-ref data 'id)
   (hash-ref data 'name null)
   (hash-ref data 'icon null)
   (hash-ref data 'splash null)
   (hash-ref data 'owner_id null)
   (hash-ref data 'region null)
   (hash-ref data 'afk_channel_id null)
   (hash-ref data 'afk_timeout null)
   (hash-ref data 'embed_enabled null)
   (hash-ref data 'embed_channel_id null)
   (hash-ref data 'verification_level null)
   (hash-ref data 'default_message_notifications null)
   (hash-ref data 'explicit_content_filter null)
   (bind make-hash (extract-and-parse data 'roles (get-id hash->role)))
   (extract-and-parse data 'emojis hash->emoji)
   (hash-ref data 'features null)
   (hash-ref data 'mfa_level null)
   (hash-ref data 'application_id null)
   (hash-ref data 'widget_enabled null)
   (hash-ref data 'widget_channel_id null)
   (hash-ref data 'joined_at null)
   (hash-ref data 'large null)
   (hash-ref data 'unavailable null)
   (hash-ref data 'member_count null)
   (hash-ref data 'voice_states null)
   (bind make-hash (extract-and-parse data 'members (lambda (data) (cons (hash-ref (hash-ref data 'user) 'id)
                                                                    (hash->member data)))))
   (bind make-hash (extract-and-parse data 'channels
                                      (get-id (lambda (d) (hash->channel d #:guild-id (hash-ref data 'id))))))
   (hash-ref data 'presences null)))

(define (hash->channel data #:guild-id [guild-id null])
  (case (hash-ref data 'type)
    [(0 2 4)
     (guild-channel
      (hash-ref data 'id)
      (hash-ref data 'type)
      (hash-ref data 'guild_id guild-id)
      (hash-ref data 'position)
      (hash-ref data 'permission_overwrites)
      (hash-ref data 'name null)
      (hash-ref data 'topic null)
      (hash-ref data 'nsfw null)
      (hash-ref data 'last_message_id null)
      (hash-ref data 'bitrate null)
      (hash-ref data 'user_limit null)
      (hash-ref data 'parent_id null))]
    [(1 3)
     (dm-channel
      (hash-ref data 'id)
      (hash-ref data 'type)
      (hash-ref data 'name null)
      (hash-ref data 'last_message_id null)
      (hash-ref data 'icon null)
      (hash-ref data 'recipients null)
      (hash-ref data 'owner_id null)
      (hash-ref data 'application_id null))]))

(define (hash->game data)
  (game
   (hash-ref data 'name)
   (hash-ref data 'type)
   (hash-ref data 'url)))

(define (game->hash game)
  (hash
   'name (game-name game)
   'type (game-type game)
   'url (game-url game)))

; TODO(williewillus) all of these updaters need to be completely redone

(define (update-guild old-guild data)
  old-guild)

(define (update-channel old-channel data)
  old-channel)

(define (update-user old-user data)
  old-user)

(define (update-member old-member data)
  old-member)


(module* test #f
  (require json
           rackunit
           "data-examples.rkt")

  (json-null #f)

  (make-test-suite
   "User deserialization"
   (list
    (test-not-exn
     "Normal user"
     (thunk (hash->user (string->jsexpr user-example))))))

  (make-test-suite
   "Member deserialization"
   (list
    (test-not-exn
     "Normal member"
     (thunk (hash->member (string->jsexpr member-example))))))

  (make-test-suite
   "Webhook deserialization"
   (list
    (test-not-exn
     "Normal webhook"
     (thunk (hash->webhook (string->jsexpr webhook-example))))))

  (make-test-suite
   "Emoji deserialization"
   (list
    (test-not-exn
     "Normal emoji"
     (thunk (hash->emoji (string->jsexpr emoji-example))))
    (test-not-exn
     "Gateway standard emoji"
     (thunk (hash->emoji (string->jsexpr emoji-gateway-standard-example))))
    (test-not-exn
     "Gateway custom emoji"
     (thunk
      (hash->emoji (string->jsexpr emoji-gateway-custom-example))
      (hash->emoji (string->jsexpr emoji-gateway-custom-example-2))))))

  (make-test-suite
   "Invite deserialization"
   (list
    (test-not-exn
     "Normal invite"
     (thunk (hash->invite (string->jsexpr invite-example))))))

  (make-test-suite
   "Role deserialization"
   (list
    (test-not-exn
     "Normal role"
     (thunk (hash->role (string->jsexpr role-example))))))
  
  (make-test-suite
   "Message deserialization"
   (list 
    (test-not-exn
     "Normal message"
     (thunk (hash->message (string->jsexpr message-example))))

    (test-not-exn
     "Crossposted message"
     (thunk (hash->message (string->jsexpr message-crosspost-example)))))))
