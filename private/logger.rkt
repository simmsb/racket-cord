#lang racket/base

(provide discord-logger
         log-discord-debug
         log-discord-error
         log-discord-info
         log-discord-warning)

(define-logger discord)
