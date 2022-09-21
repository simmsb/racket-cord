#lang racket/base

(require (only-in net/base64 base64-encode)
         (for-syntax racket/base racket/syntax syntax/parse))

(provide image-data->base64 filter-null hash-exclude-null bitflags)

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

;; A helper for defining bit flags
;; (bitflags prefix a b (c 3) d) ==>
;; (begin
;;   (define prefix-a 1) (define prefix-b 2) (define prefix-c 8) (define prefix-d 16))
(define-syntax (bitflags stx)
  (define group-prefix (cadr (syntax-e stx)))
  (define clauses
    (for/fold ([acc null]
               [next-ordinal 0]
               #:result (reverse acc))
              ([clause (in-list (cddr (syntax-e stx)))])
      (syntax-parse clause
        [id:id
         (let ([value (arithmetic-shift 1 next-ordinal)])
           (values
            (cons #`(define
                      #,(format-id #'id "~a-~a" group-prefix #'id)
                      #,value)
                  acc)
            (add1 next-ordinal)))]
        [(id:id ordinal:exact-nonnegative-integer)
         (let* ([ordinal-val (syntax-e #'ordinal)]
                [value (arithmetic-shift 1 ordinal-val)])
           (values
            (cons #`(define
                      #,(format-id #'id "~a-~a" group-prefix #'id)
                      #,value)
                  acc)
            (add1 ordinal-val)))])))
  #`(begin #,@clauses))
