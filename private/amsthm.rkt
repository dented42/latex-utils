#lang racket

(provide amsthm-path
         amsthm-style
         prefix-tag
         ref)

(require racket/runtime-path
         scribble/core
         scribble/latex-properties
         "utils.rkt")

(define-runtime-path amsthm-path "../tex/amsthm.tex")

(define amsthm-style
  (make-style "Iidentity" `(exact-chars ,(make-tex-addition id-path)
                                        ,(make-tex-addition amsthm-path))))

(define (prefix-tag tag prefix)
  (if tag
      (string-append prefix tag)
      tag))

(define (ref tag)
   (exact "\\ref{" tag "}"))