#lang racket

(require racket/hash
         "utils.rkt")

(provide (struct-out client)
         (struct-out ws-client)
         (struct-out guild)
         (struct-out channel)
         (struct-out user)
         (struct-out member)
         (struct-out message)
         (struct-out role)
         hash->guild
         hash->channel
         hash->user
         hash->member
         hash->message
         hash->role)

(struct client
  (shards
   user
   guilds
   private-channels
   event-consumer
   events
   requester
   http-loop
   token)
  #:mutable)

(struct ws-client
  ([ws #:mutable]
   token
   client
   [gateway-url #:mutable]
   shard-id
   [ready #:mutable]
   [session-id #:mutable]
   [heartbeat-thread #:mutable]
   [recv-thread #:mutable]
   [heartbeat-received #:mutable]
   [seq #:mutable]
   [reconnects #:mutable]))

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
   emojis
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
   members
   channels
   presences)
  #:mutable
  #:transparent)

(struct channel
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
   recipients
   icon
   owner-id
   application-id
   parent-id)
  #:mutable
  #:transparent)

(struct user
  (id
   username
   discriminator
   avatar
   bot
   mfa-enabled)
  #:mutable
  #:transparent)

(struct member
  (user
   nick
   roles
   joined_at
   deaf
   mute)
  #:mutable
  #:transparent)

(struct message
  (id
   channel-id
   author
   content
   timestamp
   edited-timestamp
   tts
   mention-everyone
   mentions
   mention-roles
   attachments
   embeds
   reactions
   pinned
   type)
  #:mutable
  #:transparent)

(struct role
  (id
   name
   color
   hoist
   position
   permissions
   managed
   mentionable)
  #:mutable
  #:transparent)

(define (extract-and-parse data value parser)
  (bindmap (lambda (m) (list (hash-ref data 'id) (parser m)))
           (hash-ref data value null)))

;; TODO: eventually do these as macros
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
   (extract-and-parse data 'roles hash->role)
   (hash-ref data 'emojis null)
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
   (bindap make-hash (extract-and-parse data 'members hash->member))
   (bindap make-hash (extract-and-parse data 'channels hash->channel))
   (hash-ref data 'presences null)))

(define (hash->channel data)
  (channel
   (hash-ref data 'id)
   (hash-ref data 'type null)
   (hash-ref data 'guild_id) ;; TODO: parse timestamps
   (hash-ref data 'position)
   (hash-ref data 'permission_overwrites null)
   (hash-ref data 'name)
   (hash-ref data 'topic null)
   (hash-ref data 'nsfw null)
   (hash-ref data 'last_message_id null)
   (hash-ref data 'bitrate null)
   (hash-ref data 'user_limits null)
   (hash-ref data 'recipients null)
   (hash-ref data 'icon null)
   (hash-ref data 'owner_id null)
   (hash-ref data 'application_id null)
   (hash-ref data 'parent_id null)))

(define (hash->user data)
  (user
   (hash-ref data 'id)
   (hash-ref data 'username)
   (hash-ref data 'discriminator null)
   (hash-ref data 'avatar null)
   (hash-ref data 'bot null)
   (hash-ref data 'mfa_enabled null)))

(define (hash->member data)
  (member
   (hash->user (hash-ref data 'user))
   (hash-ref data 'nick null)
   (extract-and-parse data 'roles hash->role)
   (hash-ref data 'joined_at)
   (hash-ref data 'deaf null)
   (hash-ref data 'mute null)))

(define (hash->message data)
  (message
   (hash-ref data 'id)
   (hash-ref data 'channel_id)
   (hash->user (hash-ref data 'author))
   (hash-ref data 'content)
   (hash-ref data 'timestamp)
   (hash-ref data 'edited_timestamp null)
   (hash-ref data 'tts)
   (hash-ref data 'mention_everyone)
   (extract-and-parse data 'mentions hash->user)
   (extract-and-parse data 'mention_roles hash->role)
   (hash-ref data 'attachments)
   (hash-ref data 'embeds)
   (hash-ref data 'reactions null)
   (hash-ref data 'pinned)
   (hash-ref data 'type)))

(define (hash->role data)
  (role
   (hash-ref data 'id)
   (hash-ref data 'name)
   (hash-ref data 'color)
   (hash-ref data 'hoist)
   (hash-ref data 'position)
   (hash-ref data 'permissions)
   (hash-ref data 'managed)
   (hash-ref data 'mentionable)))
