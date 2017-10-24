#lang racket

(provide (all-defined-out))

(struct client
  (shards
   user
   event-consumer
   events)
  #:mutable)
