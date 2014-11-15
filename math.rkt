#lang at-exp racket

(provide cal mcal bb mbb sf msf rm mrm)

(require "utils.rkt")

(define (cal . stuff)
  (list "\\mathcal{" stuff "}"))

(define (mcal . stuff)
  (m "\\mathcal{" stuff "}"))

(define (bb . stuff)
  (list "\\mathbb{" stuff "}"))

(define (mbb . stuff)
  (m "\\mathbb{" stuff "}"))

(define (sf . stuff)
  (list "\\mathsf{" stuff "}"))

(define (msf . stuff)
  (m "\\mathsf{" stuff "}"))

(define (rm . stuff)
  (list "\\mathrm{" stuff "}"))

(define (mrm . stuff)
  (m "\\mathrm{" stuff "}"))