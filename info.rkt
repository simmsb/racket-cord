#lang info
(define collection 'multi)
(define deps '("base"
               "simple-http"
               "rfc6455"
               "rackunit-lib"))
(define build-deps '("scribble-lib" "racket-doc"))
(define scribblings '(("scribblings/racket-cord.scrbl" ())))
(define pkg-desc "A racket wrapper for the discord API.")
(define version "0.1.0")
(define pkg-authors '(Ben Simms))
