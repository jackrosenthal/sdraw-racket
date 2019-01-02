#lang info
(define collection "sdraw")
(define deps '("base" "pict-lib"))
(define build-deps '("scribble-lib" "racket-doc" "rackunit-lib" "pict-doc"))
(define scribblings '(("scribblings/sdraw.scrbl" ())))
(define pkg-desc
  "Draw cons-cell diagrams (box-and-pointer diagrams) using pict")
(define version "0.1")
(define pkg-authors '(jrosenth))
