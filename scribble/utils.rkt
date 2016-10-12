#lang at-exp racket

(require scribble/eval scribble/core scribble/sigplan)
(require scribble/manual
         scribble/decode
         scribble/bnf
         scribble/racket
         scribble/latex-properties
         scheme/string
         racket/runtime-path
         "private/unmap.rkt"
         "private/utils.rkt"
         (for-syntax syntax/id-table syntax/parse)
         (only-in scribble/struct make-flow make-omitable-paragraph flow-paragraphs
                  make-blockquote make-styled-paragraph))

(provide enumlist
         value->content wrap
         renewcommand
         graybox ; really specific
         bracket curlies parens
         tenv
         ;; mathpartir
         mathpar
         ;; listings
         lstlisting
         lstset
         ;; pfsteps
         byCases bc-case bc-otherwise pfsteps*)

(provide/contract
 [exact (->* () (#:operators operators/c) #:rest (listof content?) content?)]
 [env (->* (content?) (#:opt (listof (or/c bracket? curlies? parens?)))
           #:rest (listof content?) content?)])

(define-runtime-path pfsteps-path "tex/pfsteps.tex")
(define-runtime-path listings-path "tex/listings.tex")
(define-runtime-path mathpar-path "tex/mathpar.tex")
(define-runtime-path bgcolor-path "tex/bgcolor.tex")


(define-runtime-path enumabc-style-tex "tex/enumerationstyles.tex")

(define enum-abc
  (make-style "enumabc" `(,(make-tex-addition enumabc-style-tex))))

(define (enumlist . item)
  (apply itemlist #:style enum-abc item))


(define listings-addition (make-tex-addition listings-path))
(define pfsteps-style
  (make-style "Iidentity" `(exact-chars ,(make-tex-addition pfsteps-path)
                            )))

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

(define (lstlisting #:math-escape? [math-escape? #f] . items)
  (list (make-element (make-style "setbox\\mylistings=\\vbox" 
                                  `(exact-chars ,listings-addition))
                      (list "\n"
                            "\\begin{lstlisting}"
                            (cond [math-escape? "[mathescape]\n"]
                                  [else "\n"])
                            (parameterize ([math-mode #t])
                              (map content->latex-content items))
                            "\n\\end{lstlisting}\n"))
        #;(make-element (make-style "copy\\mylistings" '(exact-chars)) "")
        (make-element (make-style "box\\mylistings" '(exact-chars)) "")))

(define (lstset #:basicstyle [basicstyle #f]
                #:keywordstyle [keywordstyle #f]
                #:identifierstyle [identifierstyle #f]
                #:commentstyle [commentstyle #f]
                #:stringstyle [stringstyle #f]
                #:showstringspaces [showstringspaces #f]
                #:numbers [numbers #f]
                #:numberstyle [numberstyle #f]
                #:numberblanklines [numberblanklines #f]
                #:stepnumber [stepnumber #f]
                #:numbersep [numbersep #f]
                #:backgroundcolor [backgroundcolor #f]
                #:showspaces [showspaces #f]
                #:showtabs [showtabs #f]
                #:frame [frame #f]
                #:label [label #f]
                #:rulecolor [rulecolor #f]
                #:tabsize [tabsize #f]
                #:language [language #f]
                #:caption [caption #f]
                #:captionpos [captionpos #f]
                #:breaklines [breaklines #f]
                #:breakatwhitespace [breakatwhitespace #f]
                #:title [title #f]
                #:escapeinside [escapeinside #f]
                #:morekeywords [morekeywords #f]
                #:moredelim [moredelim #f]
                #:xleftmargin [xleftmargin #f]
                #:xrightmargin [xrightmargin #f])
  (define key-values 
    `(;; styling
      ("basicstyle" . ,basicstyle)
      ("keywordstyle" . ,keywordstyle)
      ("identifierstyle" . ,identifierstyle)
      ("commentstyle" . ,commentstyle)
      ("stringstyle" . ,stringstyle)
      ;; line numbering
      ("numbers" . ,numbers)
      ("numberstyle" . ,numberstyle)
      ("numberblanklines" . ,numberblanklines)
      ("stepnumber" . ,stepnumber)
      ("numbersep" . ,numbersep)
      ;; display
      ("backgroundcolor" . ,backgroundcolor)
      ("rulecolor" . ,rulecolor)
      ("frame" . ,frame)
      ;; spacing
      ("showstringspaces" . ,showstringspaces)
      ("showspaces" . ,showspaces)
      ("showtabs" . ,showtabs)
      ("tabsize" . ,tabsize)
      ;; margins
      ("xleftmargin" . ,xleftmargin)
      ("xrightmargin" . ,xrightmargin)
      ;; line breaking
      ("breaklines" . ,breaklines)
      ("breakatwhitespace" . ,breakatwhitespace)
      ;; legend
      ("title" . ,title)
      ("caption" . ,caption)
      ("captionpos" . ,captionpos)
      ("label" . ,label)
      ;; special
      ("language" . ,language)
      ;; extra
      ("escapeinside" . ,escapeinside)
      ("morekeywords" . ,morekeywords)
      ("moredelim" . ,moredelim)))
  (make-element (make-style "lstset" `(exact-chars ,listings-addition))
                (string-join 
                 (foldr (λ (pair acc)
                           (match-define (cons key val) pair)
                           (cond [val
                                  (cons (format "~a=~a" key val)
                                        acc)]
                                 [else acc]))
                        '() key-values)
                 ",\n")))

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