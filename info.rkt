#lang info
(define collection "racket-cord")
(define deps '("base"
               "http-easy"
               "rfc6455"
               "rackunit-lib"
               "scribble-lib"))
(define build-deps '("scribble-lib" "racket-doc"))
(define scribblings '(("scribblings/racket-cord.scrbl" ())))
(define pkg-desc "A racket wrapper for the discord API.")
(define version "0.2.0")
(define pkg-authors '(Ben Simms))
