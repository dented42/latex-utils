#lang racket/base

(provide ref
         define-ref-form raw-tag raw-ref
         current-tag-normalizer
         normalize-tag
         prefix-tag)

(require racket/string
         racket/stxparam
         (for-syntax racket/base
                     syntax/parse)
         "private/utils.rkt")

(define (default-tag-normalizer tag)
  (string-foldcase (string-replace tag " " ":")))

(define current-tag-normalizer
  (make-parameter default-tag-normalizer))

(define (normalize-tag tag)
  ((current-tag-normalizer) tag))

(define (prefix-tag tag #:default (default tag) prefix)
  (if tag
      (string-append prefix tag)
      default))

(define (ref tag)
   (exact "\\ref{" tag "}"))

(define-syntax (define-ref-form stx)
  (syntax-parse stx
    [(_ name:id tag-xform:expr ref-xform:expr)
     #'(define (name tag)
         (let ([tag-ref (syntax-parameterize ([raw-tag (λ (stx) ; raw-tag -> tag
                                                         (syntax-parse stx
                                                           [_:id #'tag]))])
                          (ref tag-xform))])
           (syntax-parameterize ([raw-ref (λ (stx) ; raw-ref -> tag-ref
                                            (syntax-parse stx
                                              [_:id #'tag-ref]))])
             ref-xform)))]))

;;; only valid inside the tag-xform subform of a define-ref-form form.
(define-syntax-parameter raw-tag (syntax-rules ()))

;;; only valid inside the ref-xform subform of a define-ref-form form.
(define-syntax-parameter raw-ref (syntax-rules ()))