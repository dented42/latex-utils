#lang racket/base

(provide build-identifier)

(define (build-identifier orig . components)
  (datum->syntax orig ; provides context information
                 (string->symbol
                  (apply string-append
                         (map (λ (piece)
                                (cond
                                  [(string? piece) piece]
                                  [(identifier? piece)
                                   (symbol->string (syntax-e piece))]
                                  [(symbol? piece)
                                   (symbol->string piece)]))
                              components)))
                 orig)) ; provides location information