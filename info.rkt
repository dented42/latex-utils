#lang info

(define collection "latex-utils")

(define version "0.4.0.2")

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
