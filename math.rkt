#lang at-exp racket

(provide cal mcal bb mbb bf mbf sf msf rm mrm dd delim)

(require "utils.rkt")

(define (value->content v #:auto-wrap? (wrap? #t) #:escape? (escape? #f))
  (define (wrap . c)
    (if wrap?
        (list "{" c "}")
        c))
  (define (char->string c)
    (list->string
     (if escape?
         (match c
           [#\\ '(#\\ #\\ #\\ #\\)]
           [#\{ '(#\\ #\{)]
           [#\} '(#\\ #\})]
           [c (list c)])
         (list c))))
  (cond
    [(string? v) (wrap v)]
    [(char? v) (wrap (char->string v))]
    [(number? v) (wrap (number->string v))]))

(define (cal . stuff)
  (list "{\\mathcal{" stuff "}}"))

(define (mcal . stuff)
  (m (cal stuff)))

(define (bb . stuff)
  (list "{\\mathbb{" stuff "}}"))

(define (mbb . stuff)
  (m (bb stuff)))

(define (bf . stuff)
  (list "{\\mathbf{" stuff "}}"))

(define (mbf . stuff)
  (m (bf stuff)))

(define (sf . stuff)
  (list "{\\mathsf{" stuff "}}"))

(define (msf . stuff)
  (m (sf stuff)))

(define (rm . stuff)
  (list "{\\mathrm{" stuff "}}"))

(define (mrm . stuff)
  (m (rm stuff)))

(define (dd var . stuff)
  (list "{\\frac{d" stuff "}{d" var "}}"))

(define (delim delims . stuff)
  (let ([delims (if (and (string? delims) (= 2 (string-length delims)))
                    (string->list delims)
                    delims)])
    (list "\\left"
          (value->content (sequence-ref delims 0) #:auto-wrap? #f #:escape? #t)
          stuff
          "\\right"
          (value->content (sequence-ref delims 1) #:auto-wrap? #f #:escape? #t))))