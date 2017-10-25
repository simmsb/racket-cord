#lang racket

(require racket/hash)

(provide (struct-out client)
         (struct-out ws-client)
         (struct-out guild)
         (struct-out channel)
         (struct-out user)
         (struct-out member)
         hash->guild
         hash->channel
         hash->user
         hash->member)

(struct client
  (shards
   user
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
   gateway-url
   shard-id
   guilds
   private-channels
   [ready #:mutable]
   [session-id #:mutable]
   [heartbeat-thread #:mutable]
   [recv-thread #:mutable]
   [heartbeat-received #:mutable] ;; TODO: if delta becomes higher than 2 kill a shard
   [seq #:mutable]))

(struct guild
  (id
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

(define (extract-and-parse data value parser)
  (match (hash-ref data value null)
    [(? null?) null]
    [x (map parser x)]))

;; TODO: eventually do these as macros
(define (hash->guild data)
  (guild
   (hash-ref data 'id null)
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
   (hash-ref data 'roles null)
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
   (extract-and-parse data 'members hash->member)
   (extract-and-parse data 'channels hash->channel)
   (hash-ref data 'presences null)))

(define (hash->channel data)
  (channel
   (hash-ref data 'id)
   (hash-ref data 'type null)
   (hash-ref data 'guild_id null) ;; TODO: parse timestamps
   (hash-ref data 'position null)
   (hash-ref data 'permission_overwrites null)
   (hash-ref data 'name null)
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
   (hash-ref data 'username null)
   (hash-ref data 'discriminator null)
   (hash-ref data 'avatar null)
   (hash-ref data 'bot null)
   (hash-ref data 'mfa_enabled null)))

(define (hash->member data)
  (member
   (hash->user (hash-ref data 'user))
   (hash-ref data 'nick null)
   (hash-ref data 'roles)
   (hash-ref data 'joined_at)
   (hash-ref data 'deaf null)
   (hash-ref data 'mute null)))
