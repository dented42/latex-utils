#lang racket

(provide amsthm-path
         amsthm-style)

(require racket/runtime-path
         scribble/core
         scribble/latex-properties
         "utils.rkt")

(define-runtime-path amsthm-path "../tex/amsthm.tex")

(define amsthm-style
  (make-style "Iidentity" `(exact-chars ,(make-tex-addition id-path)
                                        ,(make-tex-addition amsthm-path))))