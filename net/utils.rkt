#lang racket

(provide bind
         bindap
         bindmap
         each
         extract-merge
         find-by-predicate
         find-key)

(define (bind f x)
  (if (null? x)
      null
      (f x)))

(define (bindap f x)
  (if (null? x)
      null
      (apply f x)))

(define (bindmap f i)
  (if (null? i)
      null
      (map f i)))

(define (each funs . args)
  (for ([i funs])
    (apply i args)))

(define (thread-each funs . args)
  (for ([i funs])
    (thread
     (thunk
      (apply i args)))))

(define (extract-merge extracter merger structs)
  (apply merger (map extracter structs)))

(define (find-by-predicate objs pred)
  (if (null? objs)
      null
      (if (pred (first objs))
          (first objs)
          (find-by-predicate (rest objs) pred))))

(define (find-key tables key)
  (if (null? tables)
      null
      (match (hash-ref (first tables) key null)
        [(? null?) (find-key (rest tables) key)]
        [x x])))
