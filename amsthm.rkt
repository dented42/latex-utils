#lang racket

(provide mdef mthm mlem mprop mnotation mcor
         parthm parunthm parlem parprop parprf
         tprf
         ntthm ntlem ntprf)

(require racket/runtime-path
         scribble/core scribble/latex-properties
         "private/utils.rkt")


(define-runtime-path amsthm-path "amsthm.tex")

(define amsthm-style
  (make-style "Iidentity" `(exact-chars ,(make-tex-addition amsthm-path))))

(define (mdef title #:tag [tag #f] . items)
  (in-style amsthm-style (tenv "definition" title (apply tagit tag items))))
(define (mthm title #:tag [tag #f] . items) 
  (in-style amsthm-style (tenv "theorem" title (apply tagit tag items))))
(define (mlem title #:tag [tag #f] . items)
  (in-style amsthm-style (tenv "lemma" title (apply tagit tag items))))
(define (mprop title #:tag [tag #f] . items)
  (in-style amsthm-style (tenv "property" title (apply tagit tag items))))
(define (mcor title  #:tag [tag #f]. items) 
  (in-style amsthm-style (tenv "corollary" title (apply tagit tag items))))
(define (mnotation title #:tag [tag #f] . items) 
  (in-style amsthm-style (tenv "notation" title (apply tagit tag items))))
(define (unthm title #:tag [tag #f] . items) 
  (in-style amsthm-style (tenv "untheorem" title (apply tagit tag items))))

(define (tprf title . items) 
  (in-style amsthm-style (tenv "proof" title items)))

(define (parthm title #:tag [tag #f] . items)
  (in-style amsthm-style (parblock "theorem" title tag items)))
(define (parlem title #:tag [tag #f] . items)
  (in-style amsthm-style (parblock "lemma" title tag items)))
(define (parunthm title #:tag [tag #f] . items)
  (in-style amsthm-style (parblock "untheorem" title tag items)))
(define (parprf #:tag [tag #f] . items)
  (in-style amsthm-style (parblock "proof" #f tag items)))
(define (parprop title #:tag [tag #f] . items)
  (in-style amsthm-style (parblock "property" title tag items)))

(define (ntthm . items) (in-style amsthm-style (apply env "theorem" items)))
(define (ntlem . items) (in-style amsthm-style (apply env "lemma" items)))
(define (ntprf . items) (in-style amsthm-style (apply env "proof" items)))