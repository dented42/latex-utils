#lang racket

(provide lstlisting
         lstset)

(require "unmap.rkt"
         racket/runtime-path
         scribble/core
         scribble/latex-properties)

(define-runtime-path listings-path "tex/listings.tex")

(define listings-addition (make-tex-addition listings-path))

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