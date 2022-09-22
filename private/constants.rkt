#lang racket/base

(require (only-in "utils.rkt" bitflags))
(provide (all-defined-out))

;; https://discord.com/developers/docs/topics/gateway#gateway-intents
(bitflags intent
          guilds
          guild-members
          guild-bans
          guild-emojis-and-stickers
          guild-integrations
          guild-webhooks
          guild-invites
          guild-voice-states
          guild-presences
          guild-messages
          guild-message-reactions
          guild-message-typing
          direct-messages
          direct-message-reactions
          direct-message-typing
          message-content
          guild-scheduled-events
          (auto-moderation-configuration 20)
          auto-moderation-execution)

;; Legacy name
(define intent-guild-emojis intent-guild-emojis-and-stickers)

;; https://discord.com/developers/docs/topics/permissions#permissions
(bitflags permission
          create-instant-invite
          kick-members
          ban-members
          administrator
          manage-channels
          manage-guild
          add-reactions
          view-audit-log
          priority-speaker
          stream
          view-channel
          send-messages
          send-tts-messages
          manage-messages
          embed-links
          attach-files
          read-message-history
          mention-everyone
          use-external-emojis
          view-guild-insights
          connect
          speak
          mute-members
          deafen-members
          move-members
          use-vad
          change-nickname
          manage-nicknames
          manage-roles
          manage-webhooks
          manage-emojis-and-stickers
          use-application-commands
          request-to-speak
          manage-events
          manage-threads
          create-public-threads
          create-private-threads
          use-external-stickers
          send-messages-in-threads
          use-embedded-activities
          moderate-members)

(define permission-manage-emojis permission-manage-emojis-and-stickers)

(define activity-type-game 0)
(define activity-type-streaming 1)
(define activity-type-listening 2)
(define activity-type-custom 4)
(define activity-type-competing 5)
