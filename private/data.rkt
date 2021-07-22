#lang racket/base

(provide (struct-out client)
         (struct-out ws-client))

(struct client
  ([shards #:mutable]
   [shard-threads #:mutable]
   [user #:mutable]
   [events #:mutable]
   http-client
   token
   intents
   running))

(struct ws-client
  (token
   client
   shard-id
   
   [gateway-url #:mutable]
   [socket #:mutable]
   [heartbeat-thread #:mutable]
   [recv-thread #:mutable]
   [heartbeat-acked #:mutable]
   [stop-channel #:mutable]

   [session-id #:mutable]
   [seq #:mutable]))
