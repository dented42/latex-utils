#lang info

(define collection "scribble-latex-utils")

(define version "0.3.0.0")

(define deps
  '("base"
    "scheme-lib"
    "scribble-lib"
    "seq-no-order"))

(define build-deps
  '("at-exp-lib"
    "racket-doc"
    "scribble-doc"))

(define scribblings '(("scribblings/utils.scrbl")))
