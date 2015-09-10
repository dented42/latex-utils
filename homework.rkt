#lang racket

(provide homework-title
         current-solution-visibility
         current-todo-visibility current-todo-warnings-enabled TODO
         mproblem msolution parproblem parsolution nproblem nsolution
         current-problem-tag-prefix current-solution-tag-prefix
         problem-ref Problem-ref problem-ref* Problem-ref*
         solution-ref Solution-ref solution-ref* Solution-ref*)

(require racket/runtime-path
         scribble/base
         scribble/core
         scribble/latex-properties
         "private/utils.rkt"
         "private/theorem.rkt"
         "references.rkt")

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

(define current-todo-visibility (make-parameter #t))
(define current-todo-warnings-enabled (make-parameter #t))

(define (TODO . t)
  (when (current-todo-warnings-enabled)
    (displayln "UNFINISHED TODOs: DO NOT TURN IN"))
  (if (current-todo-visibility)
      ((compose larger italic bold)
       (elem #:style (style #f `(,(color-property "red")))
             "TODO: " t))
      '()))



(define-amsthm-wrapper problem problem
  #:base-style homework-style
  #:auto-generate-tags #t)
(define-amsthm-wrapper solution solution
  #:base-style homework-style
  #:no-title)



(define (nproblem . items) (in-style homework-style (apply env "problem" items)))
(define (nsolution . items)
  (if (current-solution-visibility)
      (in-style homework-style (apply env "solution" items))
      '()))