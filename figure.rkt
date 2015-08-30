#lang racket/base

(provide adjust-box)

;;; Takes some content, and wraps it in something that makes the problem in
;;; http://tex.stackexchange.com/questions not an issue.
;;;
;;; Probably going to use the adjustbox LaTeX package underneath.
;;;
;;; #:h-align
;;;   - 'center : centers the content
;;;   - `(center ,width) : centers the content, constraining to a specific width. The entire box is
;;;                        then victim to the flow of LaTeX layout, I think?
;;;   - adjustbox exposes a bunch of other options, not just for horizontal alignment, but lots of
;;;     stuff. adjustbox macros take keys and values, and sometimes keys with no value indicating a
;;;     default value.
(define (adjust-box #:h-align (h-align 'center) . content)
  content)