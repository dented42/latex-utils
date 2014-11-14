#lang racket

(provide show-solutions? mproblem msolution parproblem parsolution nproblem nsolution)

(require racket/runtime-path
         scribble/core
         scribble/latex-properties
         "private/utils.rkt"
         "private/amsthm.rkt")

(define-runtime-path homework-path "tex/homework.tex")

(define homework-style
  (make-style "Iidentity" `(exact-chars ,(make-tex-addition id-path)
                                        ,(make-tex-addition amsthm-path)
                                        ,(make-tex-addition homework-path))))

(define show-solutions? (make-parameter #t))

(define (mproblem title #:tag [tag #f] . items)
  (in-style homework-style (tenv "problem" title (apply tagit tag items))))
(define (msolution title #:tag [tag #f] . items)
  (in-style homework-style (tenv "solution" title (apply tagit tag items))))

(define (parproblem title #:tag [tag #f] . items)
  (in-style homework-style (parblock "problem" title tag items)))
(define (parsolution #:tag [tag #f] . items)
  (if (show-solutions?)
      (in-style homework-style (parblock "solution" #f tag items))
      '()))

(define (nproblem . items) (in-style homework-style (apply env "problem" items)))
(define (nsolution . items) (in-style homework-style (apply env "solution" items)))