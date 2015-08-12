#lang racket

(provide homework-title
         show-solutions?
         hide-todos? silence-todo-warnings? TODO
         mproblem msolution parproblem parsolution nproblem nsolution
         problem-tag-prefix solution-tag-prefix
         problem-ref Problem-ref
         solution-ref Solution-ref)

(require racket/runtime-path
         scribble/base
         scribble/core
         scribble/latex-properties
         "private/utils.rkt"
         "private/amsthm.rkt")

(define (homework-title class  #:number (number #f) #:teacher (teacher #f) . assignment)
   (apply title `(,(bold class)
                  ,(if number (list (linebreak) "(" number ")") '())
                  ,(if teacher (list (hspace 1) (smaller (smaller (italic teacher)))) '())
                  ,(linebreak)
                  ,assignment)))

(define-runtime-path homework-path "tex/homework.tex")

(define homework-style
  (make-style "Iidentity" `(exact-chars ,(make-tex-addition id-path)
                                        ,(make-tex-addition amsthm-path)
                                        ,(make-tex-addition homework-path))))

(define show-solutions? (make-parameter #t))

(define hide-todos? (make-parameter #f))
(define silence-todo-warnings? (make-parameter #f))

(define (TODO . t)
  (when (not (silence-todo-warnings?))
    (displayln "UNFINISHED TODOs: DO NOT TURN IN"))
  (if (hide-todos?)
      '()
      ((compose larger italic bold)
       (elem #:style (style #f `(,(color-property "red")))
             "TODO: " t))))

(define problem-tag-prefix (make-parameter "problem:"))

(define solution-tag-prefix (make-parameter "solution:"))

(define (mproblem title #:tag [tag #f] . items)
  (if (show-solutions?)
      (in-style homework-style
                (tenv "problem" title (apply tagit (prefix-tag tag (problem-tag-prefix)) items)))
      '()))
(define (msolution title #:tag [tag #f] . items)
  (in-style homework-style
            (tenv "solution" title (apply tagit (prefix-tag tag (solution-tag-prefix)) items))))

(define (parproblem title #:tag [tag #f] . items)
  (in-style homework-style
            (parblock "problem" title (prefix-tag tag (problem-tag-prefix)) items)))
(define (parsolution #:tag [tag #f] . items)
  (if (show-solutions?)
      (in-style homework-style (parblock "solution" #f (prefix-tag tag (solution-tag-prefix)) items))
      '()))

(define (nproblem . items) (in-style homework-style (apply env "problem" items)))
(define (nsolution . items)
  (if (show-solutions?)
      (in-style homework-style (apply env "solution" items))
      '()))

(define (problem-ref tag) (list "problem " (ref (string-append (problem-tag-prefix) tag))))
(define (Problem-ref tag) (list "Problem " (ref (string-append (problem-tag-prefix) tag))))

(define (solution-ref tag) (list "solution " (ref (string-append (solution-tag-prefix) tag))))
(define (Solution-ref tag) (list "Solution " (ref (string-append (solution-tag-prefix) tag))))