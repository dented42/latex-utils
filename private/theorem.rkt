#lang racket/base

(provide amsthm-path
         amsthm-style
         define-amsthm-wrapper)

(require racket/runtime-path
         (for-syntax racket/base
                     racket/list
                     syntax/parse
                     racket/function)
         scribble/core
         scribble/latex-properties
         (rename-in "utils.rkt"
                    [env ntenv])
         (for-syntax "syntax.rkt")
         "../references.rkt")

(define-runtime-path amsthm-path "../tex/amsthm.tex")

(define amsthm-style
  (make-style "Iidentity" `(exact-chars ,(make-tex-addition id-path)
                                        ,(make-tex-addition amsthm-path))))


(define-syntax (when/scribble stx)
  (syntax-parse stx
      [(_ condition:expr body:expr)
       #'(if condition body '())]))

(begin-for-syntax
  (define (simple-define-ref-form-maker env-map ; remaps the env name (like from theorem to Theorem)
                                        (name-suffix "-ref") ; gets added to the end of the name
                                        (tag-seperator ":") ; used to seperate prefix from tag body
                                        (space-seperator " ")) ; seperates env name and ref# in body
    (λ (id)
      (let* ([id/str (symbol->string (syntax->datum id))]
             [name (build-identifier id (apply string-append (flatten (env-map id/str))) name-suffix)]
             [tag-seperator* (datum->syntax id tag-seperator)]
             [space-seperator* (datum->syntax id space-seperator)])
        #`(#,name
            (list #,id/str #,tag-seperator* raw-tag)
            (list #,(env-map id/str) #,space-seperator* raw-ref)))))

  (define lowercase-ref-gens (simple-define-ref-form-maker string-downcase))
  (define titlecase-ref-gens (simple-define-ref-form-maker string-titlecase)))

(define-syntax (define-amsthm-wrapper stx)
  (syntax-parse stx
    [(_ env:id shortcut:id
        (~seq (~optional (~seq #:base-style main-style:expr)
                         #:defaults ([main-style #'amsthm-style]))
              (~optional (~and #:no-title (~bind [show-title #f]))
                         #:defaults ([show-title #t]))
              (~optional (~seq #:auto-generate-tags auto-tags:boolean)
                         #:defaults ([auto-tags #'#f]))))
     (let* ([env*/sym (syntax->datum #'env)]
            [env*/str (symbol->string env*/sym)])
       (with-syntax ([env/str (datum->syntax #'env env*/str #'env)]
                     [prefix-tag-param (build-identifier #'env "current-" #'env "-tag-prefix")]
                     [autogenerate-tag-param (build-identifier #'env
                                                               "current-"
                                                               #'env
                                                               "-tag-autogeneration-enabled")]
                     [visibility-param (build-identifier #'env "current-" #'env "-visibility")]
                     [menv (build-identifier #'shortcut "m" #'shortcut)]
                     [parenv (build-identifier #'shortcut "par" #'shortcut)])
         (with-syntax ([((ref-name ref-tag-expr ref-ref-expr) ...) #`(#,(lowercase-ref-gens #'env)
                                                                      #,(titlecase-ref-gens #'env))]
                       [default-tag (if (attribute show-title) #'(if (autogenerate-tag-param)
                                                                     (normalize-tag title)
                                                                     #f) #'#f)])
           #`(begin
               ;; define tag prefix parameter
               (define prefix-tag-param (make-parameter #,(string-append env*/str ":")))
               ;; define tag autogeneration parameter
               (define autogenerate-tag-param (make-parameter auto-tags))
               ;; define visibility parameter
               (define visibility-param (make-parameter #t))
               ;;; define refs
               (define-ref-form ref-name ref-tag-expr ref-ref-expr) ...
               ;;; define raw environment wrapper
               (define (menv title #:tag [tag default-tag] . items)
                 (when (visibility-param)
                   (in-style main-style
                             (tenv env/str
                                   title
                                   (apply tagit
                                          (prefix-tag tag (prefix-tag-param))
                                          items)))))
               ;;; define paragraph wrapped environment wrapper
               (define (parenv #,@(if (attribute show-title) #'(title) #'())
                               #:tag [tag default-tag] . items)
                 (when (visibility-param)
                   (in-style main-style
                             (parblock env/str
                                       #,(if (attribute show-title) #'title #'#f)
                                       (prefix-tag tag (prefix-tag-param))
                                       items))))))))]))