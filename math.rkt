#lang at-exp racket/base

(provide #;nested-subs? sub cal mcal bb mbb bf mbf sf msf rm mrm dd delim paren
         implies forall exists one)

(require (only-in "utils.rkt" m)
         "private/math.rkt"
         "private/utils.rkt"
         racket/sequence)

(define (sub . scripts)
  (let rec ([scripts scripts])
    (if (null? scripts)
        ""
        (list "_{" (value->content (car scripts)) (rec (cdr scripts)) "}"))))

;;; this doesn't work
#;(define nested-subs?
  (make-parameter #f))
#;(define (sub . scripts)
  (let rec ([scripts (map value->content scripts)])
    (if (null? scripts)
        ""
        (if (nested-subs?)
              (list (car scripts)
                    (if (null? (cdr scripts))
                        ""
                        (list "_{" (rec (cdr scripts)) "}")))
              (if (null? (cdr scripts))
                  (list (car scripts))
                  (rec (cons (list "{" (car scripts) "_" (cadr scripts) "}") (cddr scripts))))))))

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

(define (paren . stuff)
  (delim "()" stuff))

(define implies "\\Rightarrow")

(define (forall item (set #f) (delims #f) (relation "∈"))
  (quantifier "∀" item set delims relation))

(define (exists item (set #f) (delims #f) (relation "∈"))
  (quantifier "∃" item set delims relation))

(define (one . content)
  (list "\\frac{" content "}{" content "}"))