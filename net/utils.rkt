#lang racket

(provide bind
         bindmap
         each
         extract-merge)

(define (bind f x)
  (unless (null? x)
    (f x)))

(define (bindmap f i)
  (unless (null? i)
    (map f i)))

(define (each funs . args)
  (for ([i funs])
    (apply i args)))

(define (extract-merge extracter merger structs)
  (apply merger (map extracter structs)))
