#lang at-exp racket/base

(provide m mp um
         sub
         cal mcal bb mbb bf mbf sf msf rm mrm
         dd delim implies one
         forall exists)

(require "private/math.rkt"
         "private/utils.rkt"
         "private/unmap.rkt"
         racket/sequence)

(define-syntax-rule (m items ...)
  (cond [(math-mode) (exact items ...)]
        [else (in-math (exact "$" items ... "$"))]))

(define-syntax-rule (mp items ...)
  (cond [(math-mode) (exact items ...)]
        [else (in-math (exact "\\[" items ... "\\]"))]))

(define-syntax-rule (um items ...)
  (cond [(math-mode) (unmath (exact "\\mbox{" items ... "}"))]
        [else (exact items ...)]))

(define (sub . scripts)
  (let rec ([scripts scripts])
    (if (null? scripts)
        ""
        (list "_{" (value->content (car scripts)) (rec (cdr scripts)) "}"))))

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

(define implies "\\Rightarrow")

(define (forall item (set #f) (delims #f) (relation "∈"))
  (quantifier "∀" item set delims relation))

(define (exists item (set #f) (delims #f) (relation "∈"))
  (quantifier "∃" item set delims relation))

(define (one . content)
  (list "\\frac{" content "}{" content "}"))