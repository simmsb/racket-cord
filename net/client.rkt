#lang racket

(provide (all-defined-out))

(struct client
  (shards
   user
   events)
  #:mutable)
