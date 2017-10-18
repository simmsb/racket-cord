#lang racket

(require simple-http
         racket/string)

(provide make-discord-http
         discord-url
         gateway
         gateway-bot
         create-message)

(define (make-discord-http token)
  (update-headers
   (update-ssl
    (update-host json-requester "discordapp.com")
    #t)
   (list (format "Authorization: ~a" token)
         "User-Agent: DiscordBot (https://github.com/nitros12/racket-cord, 0.0)")))

(define (discord-url . parts)
  (string-join (append '("api" "v6") parts) "/"))

(define (gateway requester)
  (get requester (discord-url "gateway")))

(define (gateway-bot requester)
  (get requester (discord-url "gateway" "bot")))

(define (create-message requester ch-id payload)
  (post requester (discord-url "channels" (number->string ch-id) "messages")
        #:data payload))
