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
   user
   guilds
   private-channels
   events
   http-client
   token
   running)
  #:mutable
  #:transparent)

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
   [seq #:mutable]))

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

(struct user
  (id
   username
   discriminator
   avatar
   bot
   mfa-enabled)
  #:transparent)

(struct member
  (user
   nick
   roles
   joined_at
   deaf
   mute
   status
   game)
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
  #:transparent)

(struct emoji
  (id
   name
   roles
   user
   require-colons
   managed)
  #:transparent)

(struct game
  (name
   type
   url)
  #:transparent)

(struct invite
  (code
   guild-id
   channel-id)
  #:transparent)

(struct webhook
  (id
   guild-id
   channel-id
   user
   name
   avatar
   token)
  #:transparent)

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
   (hash-ref data 'roles)
   (hash-ref data 'joined_at)
   (hash-ref data 'deaf null)
   (hash-ref data 'mute null)
   null
   null))

(define (hash->message data)
  (message
   (hash-ref data 'id)
   (hash-ref data 'channel_id)
   (hash->user (hash-ref data 'author))
   (hash-ref data 'content)
   (hash-ref data 'timestamp) ;; TODO: parse timestamps
   (hash-ref data 'edited_timestamp null)
   (hash-ref data 'tts)
   (hash-ref data 'mention_everyone)
   (extract-and-parse data 'mentions hash->user)
   (hash-ref data 'mention_roles)
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

(define (hash->emoji data)
  (emoji
   (hash-ref data 'id)
   (hash-ref data 'name)
   (hash-ref data 'roles null)
   (hash-ref data 'user null)
   (hash-ref data 'require_colons)
   (hash-ref data 'managed)))

(define (hash->game data)
  (if (equal? data 'null)
      null
      (game
        (hash-ref data 'name)
        (hash-ref data 'type)
        (hash-ref data 'url))))

(define (game->hash game)
  (hash
   'name (game-name game)
   'type (game-type game)
   'url (game-url game)))

(define (hash->invite data)
  (invite
   (hash-ref data 'code)
   (hash-ref (hash-ref data 'guild) 'id)
   (hash-ref (hash-ref data 'channel) 'id)))

(define (hash->webhook data)
  (webhook
   (hash-ref data 'id)
   (hash-ref data 'guild_id null)
   (hash-ref data 'channel_id)
   (bind hash->user (hash-ref data 'user null))
   (hash-ref data 'name)
   (hash-ref data 'avatar)
   (hash-ref data 'token)))

(define (update-guild old-guild data)
  (struct-copy guild old-guild
               [name (hash-ref data 'name (guild-name old-guild))]
               [icon (hash-ref data 'icon (guild-icon old-guild))]
               [splash (hash-ref data 'splash (guild-splash old-guild))]
               [owner-id (hash-ref data 'owner_id (guild-owner-id old-guild))]
               [region (hash-ref data 'region (guild-region old-guild))]
               [afk-channel-id (hash-ref data 'afk_channel_id (guild-afk-channel-id old-guild))]
               [afk-timeout (hash-ref data 'afk_timeout (guild-afk-timeout old-guild))]
               [embed-enabled (hash-ref data 'embed_enabled (guild-embed-enabled old-guild))]
               [embed-channel-id (hash-ref data 'embed_channel_id (guild-embed-channel-id old-guild))]
               [verification-level (hash-ref data 'verification_level (guild-verification-level old-guild))]
               [default-message-notifications (hash-ref data 'default_message_notifications (guild-default-message-notifications old-guild))]
               [explicit-content-filter (hash-ref data 'explicit-content-filter (guild-explicit-content-filter old-guild))]))

(define (update-channel old-channel data)
  (case (hash-ref data 'type)
    [(0 2 4)
     (struct-copy guild-channel old-channel
                  [position (hash-ref data 'position (guild-channel-position old-channel))]
                  [permission-overwrites (hash-ref data 'permission_overwrites (guild-channel-permission-overwrites old-channel))]
                  [name (hash-ref data 'name (guild-channel-name old-channel))]
                  [topic (hash-ref data 'topic (guild-channel-topic old-channel))]
                  [nsfw (hash-ref data 'nsfw (guild-channel-nsfw old-channel))]
                  [last-message-id (hash-ref data 'last_message_id (guild-channel-last-message-id old-channel))]
                  [bitrate (hash-ref data 'bitrate (guild-channel-bitrate old-channel))]
                  [user-limit (hash-ref data 'user_limit (guild-channel-user-limit old-channel))]
                  [parent-id (hash-ref data 'parent_id (guild-channel-parent-id old-channel))])]
    [(1 3)
     (struct-copy dm-channel old-channel
                  [name (hash-ref data 'name (dm-channel-name old-channel))]
                  [last-message-id (hash-ref data 'last_message_id (dm-channel-last-message-id old-channel))]
                  [recipients (if (hash-has-key? data 'recipients)
                                  (extract-and-parse data 'recipients hash->user)
                                  (dm-channel-recipients old-channel))]
                  [icon (hash-ref data 'icon (dm-channel-icon old-channel))])]))

(define (update-user old-user data)
  (struct-copy user old-user
               [username (hash-ref data 'username (user-username old-user))]
               [discriminator (hash-ref data 'discriminator (user-discriminator old-user))]
               [avatar (hash-ref data 'avatar (user-avatar old-user))]))

(define (update-member old-member data)
  (struct-copy member old-member
               [user (update-user (member-user old-member) (hash-ref data 'user))]
               [roles (hash-ref data 'roles null)]
               [status (hash-ref data 'status (member-status old-member))]
               [game (if (hash-has-key? data 'game)  ;; A bit messy
                         (hash->game (hash-ref data 'game))
                         (member-game old-member))]
               [nick (hash-ref data 'nick (member-nick old-member))]))
