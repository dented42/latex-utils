#lang racket

(provide current-style in-style
         exact-style
         (struct-out bracket)
         (struct-out curlies)
         (struct-out parens)
         interpret-option
         env tenv parblock exact tagit)

(require racket/runtime-path
         scribble/core scribble/latex-properties
         "../unmap.rkt")

;;; paths
(define-runtime-path id-path "../tex/identity.tex")

;;; style helpers
(define current-style (make-parameter exact-style))

(define-syntax-rule (in-style style body1 body ...)
  (parameterize ([current-style style]) body1 body ...))

;;; styles
(define exact-style 
  (make-style "Iidentity" `(exact-chars ,(make-tex-addition id-path))))

;;; structs
(struct bracket (element))
(struct curlies (element))
(struct parens (element))

;;; helpers
(define (interpret-option option)
  (match option
    [(bracket e) `("[" ,e "]")]
    [(curlies e) `("{" ,e "}")]
    [(parens e) `("(" ,e ")")]))

(define (content->block c)
  (if (content? c)
      (make-paragraph (current-style) c)
      c))

(define (collapse-content items)
  (let recur ([items items]
              [current '()]
              [all '()])
    (define (extend)
      (if (empty? current)
          all
          (cons (reverse current) all)))
    (cond [(empty? items) (reverse (extend))]
          [(content? (car items))
           (recur (cdr items) (cons (car items) current) all)]
          [else (recur (cdr items) '() (cons (car items) (extend)))])))

;;; latex markup
(define (exact #:operators [operators default-ops] . items)
  (make-element (current-style)
                (map (λ (i) 
                        (content->latex-content i #:operators operators))
                     items)))

(define (env t #:opt [optional '()] . items)
  (apply exact `("\\begin{" ,t "}"
                 ,@(append-map interpret-option optional)
                 ,@items
                 "\\end{" ,t "}")))

(define (tenv t title items)
  (keyword-apply env '() '() t items #:opt (list (bracket title))))

(define (parblock env title tag items)
  (define par
    (make-paragraph (current-style)
                    (exact `("\\begin{" ,env "}"
                             ,@(if title
                                   `("[" ,title "]")
                                   '())))))
  (define blocks
    (map content->block (collapse-content (apply tagit tag items))))
  (define end
    (make-paragraph (current-style)  (exact `("\\end{" ,env "}"))))
  (make-compound-paragraph (current-style)
                           (append (list par) blocks (list end))))

(define (tagit tag . items)
  (cond [tag (cons (exact `("\\label{" ,tag "}")) items)]
        [else items]))
