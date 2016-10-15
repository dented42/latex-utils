#lang at-exp racket/base

(provide m mp um
         align* envalign* style-matrix matrix
         sub
         cal mcal bb mbb bf mbf sf msf rm mrm
         delim parens
         od pd implies one
         forall exists)

(require "private/math.rkt"
         "private/utils.rkt"
         "private/unmap.rkt"
         (for-syntax racket/base
                     syntax/id-table
                     syntax/parse)
         racket/match
         racket/sequence)


; ↓↓↓↓ math modes

(define-syntax-rule (m items ...)
  (cond [(math-mode) (exact items ...)]
        [else (in-math (exact "\\(" items ... "\\)"))]))

(define-syntax-rule (mp items ...)
  (cond [(math-mode) (exact items ...)]
        [else (in-math (exact "\\[" items ... "\\]"))]))

(define-syntax-rule (um items ...)
  (cond [(math-mode) (unmath (exact "\\mbox{" items ... "}"))]
        [else (exact items ...)]))


; ↓↓↓↓ something about arrays/matrices?

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

(define (sub . scripts)
  (let rec ([scripts scripts])
    (if (null? scripts)
        ""
        (list "_{" (value->content (car scripts)) (rec (cdr scripts)) "}"))))


; ↓↓↓↓ math fonts

(define (cal . stuff)
  (list "{\\mathcal{" stuff "}}"))

(define (mcal . stuff)
  (m (cal stuff)))

(define (bb . stuff)
  (list "{\\mathbb{" stuff "}}"))

(define (mbb . stuff)
  (m (bb stuff)))

(define (bf . stuff)
  (list "{\\mathbf{" stuff "}}"))

(define (mbf . stuff)
  (m (bf stuff)))

(define (sf . stuff)
  (list "{\\mathsf{" stuff "}}"))

(define (msf . stuff)
  (m (sf stuff)))

(define (rm . stuff)
  (list "{\\mathrm{" stuff "}}"))

(define (mrm . stuff)
  (m (rm stuff)))


; ↓↓↓↓ math delimiters

(define (delim delims . stuff)
  (let ([delims (if (string? delims)
                    (cond
                      [(= 2 (string-length delims)) (string->list delims)]
                      [(string=? "{}") (list "\\{" "\\}")])
                    delims)])
    (list "\\left"
          (value->content (sequence-ref delims 0) #:auto-wrap? #f #:escape? #t)
          stuff
          "\\right"
          (value->content (sequence-ref delims 1) #:auto-wrap? #f #:escape? #t))))

(define (parens . stuff)
  (delim "()" stuff))

; add more syntactic sugar for common delimiters?


; ↓↓↓↓ miscellaneous math objects

(define implies "\\Rightarrow") ; redundant with LaTeX \implies

(define (one . content)
  (list "\\frac{" content "}{" content "}"))

(define (od var . stuff) ; need to add handling for optional order
  (list "{\\frac{\\mathrm{d}" stuff "}{\\mathrm{d}" var "}}"))

(define (pd var . stuff) ; need to add handling for optional order
  (list "{\\frac{\\partial" stuff "}{\\partial" var "}}"))


(define (forall item (set #f) (delims #f) (relation "∈"))
  (quantifier "∀" item set delims relation))

(define (exists item (set #f) (delims #f) (relation "∈"))
  (quantifier "∃" item set delims relation))