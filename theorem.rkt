#lang racket

(provide mdef mthm mlem mprop mnotation mcor mconj
         pardef parthm parconj parlem parprop parnotation parcor parprf
         tprf
         ntthm ntlem ntprf
         current-definition-tag-prefix current-theorem-tag-prefix current-lemma-tag-prefix
         current-property-tag-prefix current-corollary-tag-prefix current-notation-tag-prefix
         current-conjecture-tag-prefix current-proof-tag-prefix
         definition-ref Definition-ref
         theorem-ref Theorem-ref
         lemma-ref Lemma-ref
         property-ref Property-ref
         corollary-ref Corollary-ref
         notation-ref Notation-ref
         conjecture-ref Conjecture-ref
         proof-ref Proof-ref)

(require "private/utils.rkt"
         "private/theorem.rkt"
         "references.rkt")

(define-amsthm-wrapper definition def)
(define-amsthm-wrapper theorem thm)
(define-amsthm-wrapper lemma lem)
(define-amsthm-wrapper property prop)
(define-amsthm-wrapper corollary cor)
(define-amsthm-wrapper notation notation)
(define-amsthm-wrapper conjecture conj)
(define-amsthm-wrapper proof prf #:no-title)

(define (tprf title . items)
  (if (current-proof-visibility)
      (in-style amsthm-style (tenv "proof" title items))
      '()))

(define (ntthm . items) (in-style amsthm-style (apply env "theorem" items)))
(define (ntlem . items) (in-style amsthm-style (apply env "lemma" items)))
(define (ntprf . items) (in-style amsthm-style (apply env "proof" items)))