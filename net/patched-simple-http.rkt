#lang racket

(require (except-in simple-http
                    get post put patch delete
                    exn:fail:network:http:error)
         html-parsing
         xml
         net/http-client
         net/uri-codec
         json)

(provide get
         post
         put
         patch
         delete
         (struct-out exn:fail:network:http:error))


(define (map-headers headers)
  (define (make-pairs s)
    (let* ([key (car (regexp-match #rx"[A-Za-z-]*:" s))]
           [val (string-replace s key "")])
      (list (string->symbol
             (string-titlecase (string-trim (string-replace key ":" ""))))
            (string-trim val))))
  (make-immutable-hasheq
   (map (λ (s)
          (make-pairs
           (if (bytes? s)
               (bytes->string/utf-8 s)
               s)))
        headers)))

(define (correct-content-type? requester resp-headers)
  (let* ([resh (map-headers resp-headers)]
         [accept (requester-accept-header requester)]
         [ctype (hash-ref resh 'Content-Type)])
    (regexp-match (regexp accept) (car ctype))))


(define (requester-accept-header requester)
  (let ([type (requester-type requester)])
    (cond
      [(eq? type 'html) "text/html"]
      [(eq? type 'json) "application/json"]
      [(eq? type 'xml) "text/xml"]
      [(eq? type 'text) "text/plain"]
      [else "*"])))

(define (make-http-read-exn headers response)
  (let ([type (get-content-type headers)]
        [body (port->string response)])
    (raise
     (exn:fail:network:http:read
      (~a body) (current-continuation-marks) type))))

(define (create-response status headers response)
  (with-handlers ([exn:fail? (λ (e) (make-http-read-exn headers response))])
    (let ([type (get-content-type headers)])
      (cond
        [(eq? type 'json)
         (json-response status headers (read-json response))]
        [(eq? type 'html)
         (html-response status headers (html->xexp response))]
        [(eq? type 'xml)
         (xml-response status headers (read-xml response))]
        [else
         (text-response status headers (port->string response))]))))

(define (update-headers req nheaders)
  (struct-copy requester req
               [headers (merge-headers
                         nheaders
                         (requester-headers req))]))

(define (merge-headers . hs)
  (let* ([headers (apply append (reverse hs))]
         [hm (map-headers headers)]
         [hl (hash->list hm)])
    (sort (map (λ (s) (format "~a: ~a" (car s) (cadr s))) hl) string<?)))

;; Regex Content-Type header to figure out what we have
(define (get-content-type headers-hash)
  (match (car (hash-ref headers-hash 'Content-Type))
    [(regexp #rx"application/json") 'json]
    [(regexp #rx"text/html") 'html]
    [(regexp #rx"application/xml") 'xml]
    [else 'text]))

(struct exn:fail:network:http:error exn:fail:network (code type headers) #:transparent)

(define (make-http-error-exn code headers response)
  (let* ([type (get-content-type headers)]
         [body (cond
                 [(eq? type 'json) (read-json response)]
                 [(eq? type 'html) (html->xexp response)]
                 [(eq? type 'xml) (read-xml response)]
                 [else (port->string response)])])
    (raise
     (exn:fail:network:http:error
      (~a body) (current-continuation-marks) code type headers))))

(define (make-uri uri [params '()])
  (format "~a~a" uri (params->string params)))

(define (params->string ps)
  (if (not (empty? ps))
      (format "?~a" (alist->form-urlencoded ps))
      ""))

(define (get-status-code status-bytes)
  (string->number
   (car (regexp-match
         #px"\\d{3}"
         (bytes->string/utf-8 status-bytes)))))

(define-syntax (define-http-method-give-me-headers stx)
  (syntax-case stx ()
    [(_ method verb)
     #'(define (method req uri #:data [data #f] #:params [params '()])
         (let ([nuri (make-uri uri params)]
               [host (requester-host req)]
               [headers (requester-headers req)]
               [ssl (requester-ssl req)])
           (let-values ([(status resp-headers response)
                         (http-sendrecv host nuri #:ssl? ssl #:method verb
                                        #:headers headers #:data data)])
             (define response-code (get-status-code status))

             ;; Raise HTTP exn if we get an HTTP error code
             (when (response-code . > . 399)
               (make-http-error-exn
                response-code
                (map-headers resp-headers)
                response))

             ;; Raise read exn if the requested content type doesn't match
             ;; the response content type and isn't a redirect
             (when
                 (and
                  (or (< response-code 300) (> response-code 309))
                  (false? (correct-content-type? req resp-headers)))
               (make-http-read-exn (map-headers resp-headers) response))
             (create-response
              (bytes->string/utf-8 status) (map-headers resp-headers) response))))]))

(define-http-method-give-me-headers get '"GET")
(define-http-method-give-me-headers post '"POST")
(define-http-method-give-me-headers put '"PUT")
(define-http-method-give-me-headers patch '"PATCH")
(define-http-method-give-me-headers delete '"DELETE")
