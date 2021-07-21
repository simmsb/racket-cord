#lang racket

(require scribble/srcdoc
         (for-doc scribble/base scribble/manual)
         (for-label "data.rkt"))

(require "constants.rkt"
         "data.rkt"
         "private/logger.rkt"
         "events.rkt"
         (only-in "gateway.rkt" new-ws-client send-status-update start-shard stop-shard)
         (prefix-in http: "http.rkt"))

(provide on-event
         (all-from-out "constants.rkt")
         (all-from-out "http.rkt")
         (struct-out client)
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
         [http-client (http:make-http-client token)])
    (let-values ([(ws-url shards)
                  (if auto-shard
                      (http:get-ws-url-bot http-client)
                      (values (http:get-ws-url http-client)
                              shard-count))])
      (new-client
       (map (lambda (n) (new-ws-client client n ws-url))
            (range shards))
       http-client token intents))))

(define (new-client shards http-client token intents)
  (let ([clnt
         (client
          shards ; shard ws-clients
          null ; shard-threads
          #f ; user
          #hash() ; event handlers
          http-client
          token
          intents
          (make-semaphore 0))])
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
