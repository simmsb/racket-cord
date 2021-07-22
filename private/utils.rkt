#lang racket/base

(require (only-in net/base64 base64-encode)
         (for-syntax racket/base))

(provide image-data->base64 filter-null hash-exclude-null)

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
