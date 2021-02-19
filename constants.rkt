#lang racket/base

(provide (all-defined-out))

(define intent-guilds (arithmetic-shift 1 0))
(define intent-guild-members (arithmetic-shift 1 1))
(define intent-guild-bans (arithmetic-shift 1 2))
(define intent-guild-emojis (arithmetic-shift 1 3))
(define intent-guild-integrations (arithmetic-shift 1 4))
(define intent-guild-webhooks (arithmetic-shift 1 5))
(define intent-guild-invites (arithmetic-shift 1 6))
(define intent-guild-voice-states (arithmetic-shift 1 7))
(define intent-guild-presences (arithmetic-shift 1 8))
(define intent-guild-messages (arithmetic-shift 1 9))
(define intent-guild-message-reactions (arithmetic-shift 1 10))
(define intent-guild-message-typing (arithmetic-shift 1 11))
(define intent-direct-messages (arithmetic-shift 1 12))
(define intent-direct-message-reactions (arithmetic-shift 1 13))
(define intent-direct-message-typing (arithmetic-shift 1 14))
