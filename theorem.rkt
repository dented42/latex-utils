#lang racket

(provide mdef mthm mlem mprop mnotation mcor
         pardef parthm parunthm parlem parprop parnotation parcor parprf
         tprf
         ntthm ntlem ntprf
         def-tag-prefix thm-tag-prefix lem-tag-prefix prop-tag-prefix cor-tag-prefix
         notation-tag-prefix unthm-tag-prefix prf-tag-prefix
         definition-ref Definition-ref
         theorem-ref Theorem-ref
         lemma-ref Lemma-ref
         property-ref Property-ref
         corollary-ref Corollary-ref
         notation-ref Notation-ref
         conjecture-ref Conjecture-ref
         proof-ref Proof-ref)

(require "private/utils.rkt"
         "private/theorem.rkt")

(define def-tag-prefix (make-parameter "def:"))
(define thm-tag-prefix (make-parameter "thm:"))
(define lem-tag-prefix (make-parameter "lem:"))
(define prop-tag-prefix (make-parameter "prop:"))
(define cor-tag-prefix (make-parameter "cor:"))
(define notation-tag-prefix (make-parameter "notation:"))
(define unthm-tag-prefix (make-parameter "unthm:"))
(define prf-tag-prefix (make-parameter "prf:"))

;;; TODO: Make these consistent.

(define (mdef title #:tag [tag #f] . items)
  (in-style amsthm-style
            (tenv "definition" title (apply tagit (prefix-tag tag (def-tag-prefix)) items))))
(define (mthm title #:tag [tag #f] . items) 
  (in-style amsthm-style
            (tenv "theorem" title (apply tagit (prefix-tag tag (thm-tag-prefix)) items))))
(define (mlem title #:tag [tag #f] . items)
  (in-style amsthm-style
            (tenv "lemma" title (apply tagit (prefix-tag tag (lem-tag-prefix)) items))))
(define (mprop title #:tag [tag #f] . items)
  (in-style amsthm-style
            (tenv "property" title (apply tagit (prefix-tag tag (prop-tag-prefix)) items))))
(define (mcor title  #:tag [tag #f]. items) 
  (in-style amsthm-style
            (tenv "corollary" title (apply tagit (prefix-tag tag (cor-tag-prefix)) items))))
(define (mnotation title #:tag [tag #f] . items) 
  (in-style amsthm-style
            (tenv "notation" title (apply tagit (prefix-tag tag (notation-tag-prefix)) items))))
(define (unthm title #:tag [tag #f] . items) 
  (in-style amsthm-style
            (tenv "untheorem" title (apply tagit (prefix-tag tag (unthm-tag-prefix)) items))))

(define (tprf title . items) 
  (in-style amsthm-style (tenv "proof" title items)))

(define (pardef title #:tag [tag #f] . items)
  (in-style amsthm-style (parblock "definition" title (prefix-tag tag (def-tag-prefix)) items)))
(define (parthm title #:tag [tag #f] . items)
  (in-style amsthm-style (parblock "theorem" title (prefix-tag tag (thm-tag-prefix)) items)))
(define (parlem title #:tag [tag #f] . items)
  (in-style amsthm-style (parblock "lemma" title (prefix-tag tag (lem-tag-prefix)) items)))
(define (parunthm title #:tag [tag #f] . items)
  (in-style amsthm-style (parblock "untheorem" title (prefix-tag tag (unthm-tag-prefix)) items)))
(define (parprf #:tag [tag #f] . items)
  (in-style amsthm-style (parblock "proof" #f (prefix-tag tag (prf-tag-prefix)) items)))
(define (parprop title #:tag [tag #f] . items)
  (in-style amsthm-style (parblock "property" title (prefix-tag tag (prop-tag-prefix)) items)))
(define (parnotation title #:tag [tag #f] . items)
  (in-style amsthm-style (parblock "notation" title (prefix-tag tag (notation-tag-prefix)) items)))
(define (parcor title  #:tag [tag #f]. items) 
  (in-style amsthm-style (parblock "corollary" title (prefix-tag tag (cor-tag-prefix)) items)))

(define (ntthm . items) (in-style amsthm-style (apply env "theorem" items)))
(define (ntlem . items) (in-style amsthm-style (apply env "lemma" items)))
(define (ntprf . items) (in-style amsthm-style (apply env "proof" items)))

(define (definition-ref tag) (list "definition " (ref (string-append (def-tag-prefix) tag))))
(define (Definition-ref tag) (list "Definition " (ref (string-append (def-tag-prefix) tag))))

(define (theorem-ref tag) (list "theorem " (ref (string-append (thm-tag-prefix) tag))))
(define (Theorem-ref tag) (list "Theorem " (ref (string-append (thm-tag-prefix) tag))))

(define (lemma-ref tag) (list "lemma " (ref (string-append (lem-tag-prefix) tag))))
(define (Lemma-ref tag) (list "Lemma " (ref (string-append (lem-tag-prefix) tag))))

(define (property-ref tag) (list "property " (ref (string-append (prop-tag-prefix) tag))))
(define (Property-ref tag) (list "Property " (ref (string-append (prop-tag-prefix) tag))))

(define (corollary-ref tag) (list "corollary " (ref (string-append (cor-tag-prefix) tag))))
(define (Corollary-ref tag) (list "Corollary " (ref (string-append (cor-tag-prefix) tag))))

(define (notation-ref tag) (list "notation " (ref (string-append (notation-tag-prefix) tag))))
(define (Notation-ref tag) (list "Notation " (ref (string-append (notation-tag-prefix) tag))))

(define (conjecture-ref tag) (list "conjecture " (ref (string-append (unthm-tag-prefix) tag))))
(define (Conjecture-ref tag) (list "Conjecture " (ref (string-append (unthm-tag-prefix) tag))))

(define (proof-ref tag) (list "proof " (ref (string-append (prf-tag-prefix) tag))))
(define (Proof-ref tag) (list "Proof " (ref (string-append (prf-tag-prefix) tag))))