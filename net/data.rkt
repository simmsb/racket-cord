#lang racket

(require racket/hash)

(provide (all-defined-out))

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
   [heartbeat-delta #:mutable]
   [seq #:mutable]))

(define (maybe-parse data parser)
  (if (null? data)
      null
      (parser data)))

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
  (id)
  #:mutable
  #:transparent) ;; TODO

;; TODO: eventually do these as macros
(define (hash->guild data)
  (guild
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
    (maybe-parse (hash-ref data 'members null) (lambda (d) (map hash->member d))) ;; like this?
    (hash-ref data 'channels null)
    (hash-ref data 'presences null)))

(define (hash->channel data)
  (channel
   (hash-ref data 'id)
   (hash-ref data 'type null)
   (hash-ref data 'guild_id)
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
   (hash-ref data 'id))) ;; TODO
