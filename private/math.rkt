#lang racket

(provide current-quantifier-delimiters
         quantifier)

(require "utils.rkt")

(define current-quantifier-delimiters
  (make-parameter "()"))

;;; if set is #f then there is no set or relation.
;;; delims can be #f which defaults to (current-quantifier-delimiters)
(define (quantifier quant item set delims relation)
  (let ([opening-delim (sequence-ref (or delims (current-quantifier-delimiters)) 0)]
        [closing-delim (sequence-ref (or delims (current-quantifier-delimiters)) 1)])
    (wrap
     (list (value->content opening-delim)
           (value->content quant) (value->content "\\ ") (value->content item)
           (if set
               (list (value->content relation) (value->content set))
               '())
           (value->content closing-delim)))))