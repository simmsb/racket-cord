#lang racket

(require scribble/srcdoc
         (for-doc scribble/base scribble/manual)
         (for-label "private/data.rkt"))

(require "private/constants.rkt"
         "private/data.rkt"
         "private/events.rkt"
         (only-in "private/gateway.rkt" new-ws-client send-status-update start-shard stop-shard)
         "private/logger.rkt"
         (prefix-in http: "http.rkt"))

(provide on-event
         (all-from-out "private/constants.rkt")
         (all-from-out "http.rkt")
         client?
         client-user
         client-intents
         client-events
         discord-logger)

(define (format-token token type)
  (case type
    [(bot)    (string-append "Bot "    token)]
    [(bearer) (string-append "Bearer " token)]
    [(client) token]
    [else (raise-user-error 'format-token "~a is not a valid token type" type)]))

(provide
 (proc-doc/names
  make-client (->* (string?)
                   (#:intents (listof integer?) ; todo narrow to just known intents?
                    #:token-type (or/c 'bot 'bearer 'client)
                    #:auto-shard boolean?
                    #:shard-count integer?)
                   client?)
  ((token) ((intents null) (token-type 'bot) (auto-shard #f) (shard-count 1)))
  ("Constructs a client with the passed token."
   (linebreak)
   (linebreak)
   (racket #:auto-shard) ": If " (racket #t) ", ask Discord what the number of shards should be. Only applies if "
   (racket #:token-type) " is " (racket 'bot) "."
   (linebreak)
   (linebreak)
   (racket #:shard-count) ": If " (racket #:auto-shard) " is " (racket #f)
   ", the number of shards to use.")))
(define (make-client token
                     #:intents [intents null]
                     #:token-type [token-type 'bot]
                     #:auto-shard [auto-shard #f]
                     #:shard-count [shard-count 1])
  (let* ([fmt-token (format-token token token-type)]
         [http-client (http:make-http-client fmt-token)])
    (let-values ([(ws-url shards)
                  (if auto-shard
                      (http:get-ws-url-bot http-client)
                      (values (http:get-ws-url http-client)
                              shard-count))])
      (new-client ws-url shards http-client fmt-token intents))))

(define (new-client ws-url shard-count http-client token intents)
  (let ([clnt
         (client
          null ; shard ws-clients
          null ; shard-threads
          #hash() ; user
          #hash() ; event handlers
          http-client
          token
          intents
          (make-semaphore 0))])
    (set-client-shards!
     clnt 
     (map (lambda (n) (new-ws-client clnt n ws-url))
          (range shard-count)))
    (add-events clnt)
    clnt))

(provide
 (proc-doc/names
  start-client (-> client? void?) (client)
  ("Starts a client and begins handling events."
   (linebreak)
   "This function blocks until the client is stopped through " (racket stop-client) ".")))
(define (start-client client)
  (start-client-no-wait client) 
  (semaphore-wait (client-running client)))

(provide
 (proc-doc/names
  start-client-no-wait (-> client? void?) (client)
  ("Same as " (racket start-client) " but does not block the calling thread.")))
(define (start-client-no-wait client)
  (set-client-shard-threads!
   client
   (map start-shard (client-shards client))))

(provide
 (proc-doc/names
  stop-client (-> client? void?) (client)
  ("Stops a client")))
(define (stop-client client)
  (for ([shard (client-shard-threads client)])
    (stop-shard shard))
  (semaphore-post (client-running client)))

(provide
 (proc-doc/names
  update-status (->* (client?)
                     (#:since (or/c integer? #f)
                      #:activities (listof hash?)
                      #:status (or/c "online" "dnd" "idle" "invisible" "offline") ; TODO: make it use symbols?
                      #:afk boolean?)
                     void?)
  ((client) ((since #f) (activities null) (status "online") (afk #f)))
  ("Updates the client's status.")))
(define (update-status client
                       #:since [since #f]
                       #:activities [activities null]
                       #:status [status "online"]
                       #:afk [afk #f])
  ; XXX not quite right?
  (for ([shard (client-shards client)])
    (send-status-update shard status afk activities since)))
