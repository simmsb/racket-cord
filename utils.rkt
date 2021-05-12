#lang racket/base

(require net/base64
         racket/function
         racket/list
         racket/match
         (for-syntax racket/base))

(provide (all-defined-out))

(define-logger discord)

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

(define (image-data->base64 type data)
  (format "data:image/~a;base64,~a"
          type
          (base64-encode data)))

(define (filter-null lst)
  (filter (lambda (i) (not (null? (cdr i)))) lst))

(define-syntax (hash-exclude-null-helper stx)
  (syntax-case stx ()
    [(_ d k v)
     #'(unless (null? v)
         (hash-set! d k v))]
    [(_ d k v r ...)
     #'((unless (null? v)
          (hash-set! d k v))
        (hash-exclude-null-helper d r ...))]))

(define-syntax (hash-exclude-null stx)
  (syntax-case stx ()
    [(_ r ...)
     #'(let ([data (make-hash)])
         (hash-exclude-null-helper data r ...))]))
