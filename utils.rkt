#lang at-exp racket

(require scribble/core
         scribble/latex-properties
         racket/string
         racket/runtime-path
         "unmap.rkt"
         "private/utils.rkt"
         (for-syntax syntax/id-table syntax/parse)
         (only-in scribble/struct make-flow make-omitable-paragraph flow-paragraphs
                  make-blockquote make-styled-paragraph))

(provide m mp um renewcommand
         graybox ; really specific
         bracket curlies parens
         tenv
         align* envalign*
         array style-matrix matrix
         ;; mathpartir
         mathpar
         ;; pfsteps
         byCases bc-case bc-otherwise pfsteps*)

(provide/contract
 [exact (->* () (#:operators operators/c) #:rest (listof content?) content?)]
 [env (->* (content?) (#:opt (listof (or/c bracket? curlies? parens?)))
           #:rest (listof content?) content?)])

(define-runtime-path pfsteps-path "tex/pfsteps.tex")
(define-runtime-path mathpar-path "tex/mathpar.tex")
(define-runtime-path bgcolor-path "tex/bgcolor.tex")

(define pfsteps-style
  (make-style "Iidentity" `(exact-chars ,(make-tex-addition pfsteps-path)
                            )))

(define-syntax-rule (m items ...)
  (cond [(math-mode) (exact items ...)]
        [else (in-math (exact "$" items ... "$"))]))
(define-syntax-rule (mp items ...)
  (cond [(math-mode) (exact items ...)]
        [else (in-math (exact "\\[" items ... "\\]"))]))
(define-syntax-rule (um items ...)
  (cond [(math-mode) (unmath (exact "\\mbox{" items ... "}"))]
        [else (exact items ...)]))

(define (renewcommand item0 item1)
  (make-multiarg-element (make-style "renewcommand" '(exact-chars)) (list item0 item1)))

(define bg-color-style
  (make-style "BgColor" (list (make-tex-addition bgcolor-path))))
(define (graybox elm) (make-element bg-color-style elm))

(define-syntax-rule (mathpar items ...)
  (list (make-element (make-style "setbox\\mymathpar=\\vbox" 
                                  `(exact-chars ,(make-tex-addition mathpar-path)
                                    ))
                      (list "\n"
                            "\\begin{mathpar}"
                            (parameterize ([math-mode #t])
                              (map content->latex-content (list items ...)))
                            "\n\\end{mathpar}\n"))
        (make-element (make-style "box\\mymathpar" '(exact-chars)) "")))

(define (array style . items)
  (keyword-apply env '() '() "array" items #:opt (list (curlies style))))

;; For working with Jesse's pfsteps library
(define-syntax-rule (byCases items ...)
  (in-style pfsteps-style (env "byCases" items ...)))
(define-syntax-rule (pfsteps* items ...) 
  (in-style pfsteps-style (env "pfsteps*" items ...)))
(define-syntax-rule (bc-case title items ...)
  (in-style pfsteps-style (exact "\\case{" (in-math (um title)) "}" items ...)))
(define-syntax-rule (bc-otherwise items ...)
  (in-style pfsteps-style (exact `("\\otherwise{}" items ...))))

(define-syntax (sep-rows stx)
  (syntax-case stx ()
    [(_ (args ...) ...)
     (let ([rows (reverse
                  (let recur ([rows (map syntax->list (syntax->list #'((args ...) ...)))]
                              [acc '()])
                    (define (dorow row last? acc)
                      (cond [(null? row) acc]
                            [(null? (cdr row))
                             (cond [last? (cons (car row) acc)]
                                   [else (list* "\\\\" (car row) acc)])]
                            [else (dorow (cdr row) last? (list* "&" (car row) acc))]))
                    (cond [(null? rows) acc]
                          [else (recur (cdr rows)
                                       (append (dorow (car rows) (null? (cdr rows)) '())
                                               acc))])))])
       (quasisyntax/loc stx (#,@rows)))]))

(define-syntax (align* stx)
  (syntax-case stx ()
    [(_ (args ...) ...)
     (let ([rows (local-expand #'(sep-rows (args ...) ...) 'expression #f)])
       #`(envalign* #,@rows))]))

;; align* is a special LaTeX environment that puts its body directly in a math mode context.
(define-syntax-rule (envalign* items ...)
  (in-math (env "align*" items ...)))

(define-syntax (style-matrix stx)
  (define lut (make-free-id-table (hasheq #'l #\l #'r #\r #'c #\c #'bar #\|)))
  (define-syntax-class style
    #:attributes (kind)
    (pattern x:id #:attr kind (free-id-table-ref lut #'x #f) #:when (attribute kind)))
  (syntax-parse stx
    [(_ (s:style ...) (args ...) ...)
     (let* ([argss (map syntax->list (syntax->list #'((args ...) ...)))]
            [n (length argss)]
            [_ (when (zero? n) (raise-syntax-error #f "matrix needs at least one row." stx))]
            [m (length (car argss))]
            [ss (attribute s.kind)])
       (unless (for/and ([arg (in-list (cdr argss))])
                 (= m (length arg)))
         (raise-syntax-error #f "matrix needs same number of columns in each row." stx))
       (let ([rows (local-expand #'(sep-rows (args ...) ...) 'expression #f)])
         (quasisyntax/loc stx (array #,(list->string ss) #,@rows))))]))

(define-syntax (matrix stx)
  (syntax-case stx ()
    [(_ (args ...) ...)
     (let* ([argss (map syntax->list (syntax->list #'((args ...) ...)))]
            [_ (when (null? argss) (raise-syntax-error #f "matrix needs at least one row." stx))]
            [m (length (car argss))]
            [style (datum->syntax stx (build-list m (λ _ #'l)))])
       (quasisyntax/loc stx (style-matrix #,style (args ...) ...)))]))
