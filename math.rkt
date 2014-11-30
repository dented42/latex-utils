#lang at-exp racket

(provide cal mcal bb mbb bf mbf sf msf rm mrm dd delim)

(require "utils.rkt")

(define (value->content v)
  (cond
    [(string? v) (list "{" v "}")]
    [(char? v) (list->string (list v))]
    [(number? v) (list "{" (number->string v) "}")]))

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
  (list "\\left"
        (value->content (sequence-ref delims 0))
        stuff
        "\\right"
        (value->content (sequence-ref delims 1))))