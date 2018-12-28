#lang racket/base
;; Inspired by the SDRAW program from David S. Touretzky's
;; "Common Lisp: A Gentle Introduction to Symbolic Computation"
;;
;; This program draws cons-cell diagrams using Racket's pict library
;; Example usage: (sdraw '(1 2 (+ 3 4) 5))
;;
;; Author: Jack Rosenthal <jack@rosenth.al>
;; This program is licensed under the MIT License. You should have
;; received a file "LICENSE" with this program explaining the details.

(require racket/contract)
(require pict)
(require pict/code)
(require pict/convert)
(require pict/color)
(provide sdraw)

(define default-etc-pict (text "etc." '() 24))

(define/contract (sdraw obj
                        #:cell-border-color [cell-border-color "Black"]
                        #:cell-inside-color [cell-inside-color "White"]
                        #:arrow-color [arrow-color "Black"]
                        #:arrow-thickness [arrow-thickness 5]
                        #:arrow-head-size [arrow-head-size 15]
                        #:arrow-point-size [arrow-point-size 15]
                        #:object-padding [object-padding 2]
                        #:text-scale [text-scale 2.0]
                        #:cell-inside-size [cell-inside-size 30]
                        #:cell-inside-radius [cell-inside-radius 3]
                        #:cell-border-radius [cell-border-radius 3]
                        #:cell-border-width [cell-border-width 4]
                        #:vertical-spacing [vertical-spacing 50]
                        #:horizontal-spacing [horizontal-spacing 80]
                        #:etc-pict [etc-pict default-etc-pict]
                        #:max-depth [max-depth +inf.0]
                        #:max-width [max-width +inf.0])
  (->* (any/c)
       (#:cell-border-color color/c
        #:cell-inside-color color/c
        #:arrow-color color/c
        #:arrow-thickness real?
        #:arrow-head-size real?
        #:arrow-point-size real?
        #:object-padding real?
        #:text-scale real?
        #:cell-inside-size real?
        #:cell-inside-radius real?
        #:cell-border-radius real?
        #:cell-border-width real?
        #:vertical-spacing real?
        #:horizontal-spacing real?
        #:etc-pict pict?
        #:max-depth (or/c +inf.0 natural-number/c)
        #:max-width (or/c +inf.0 natural-number/c))
       pict?)
  (define (cons-part)
    (cc-superimpose
     (filled-rounded-rectangle cell-inside-size cell-inside-size
                               cell-inside-radius
                               #:color cell-inside-color
                               #:draw-border? #f)
     (filled-ellipse arrow-point-size arrow-point-size
                     #:color arrow-color
                     #:draw-border? #f)))
  (define (rec obj depth width)
    (cond
      [(or (> depth max-depth)
           (> width max-width))
       (rec etc-pict 0 0)]
      [(pair? obj)
       (let*-values ([(car-part) (cons-part)]
                     [(cdr-part) (cons-part)]
                     [(cell-inner) (hc-append cell-border-width
                                              car-part cdr-part)]
                     [(cell-body) (cc-superimpose
                                   (scale-to-fit
                                    (filled-rounded-rectangle
                                     (pict-width cell-inner)
                                     (pict-height cell-inner)
                                     cell-border-radius
                                     #:draw-border? #f
                                     #:color cell-border-color)
                                    (+ (pict-width cell-inner)
                                       (* 2 cell-border-width))
                                    (+ (pict-height cell-inner)
                                       (* 2 cell-border-width))
                                    #:mode 'distort)
                                   cell-inner)]
                     [(car-drawn car-attach _1) (rec (car obj)
                                                     (add1 depth)
                                                     width)]
                     [(cdr-drawn _2 cdr-attach) (rec (cdr obj)
                                                     depth
                                                     (add1 width))]
                     [(attach-x _3) (rb-find car-drawn car-attach)]
                     [(_4 attach-y) (rb-find cdr-drawn cdr-attach)]
                     [(car-point-x car-point-y) (cc-find cell-body car-part)]
                     [(ima-car-attach) (blank car-point-x 0)]
                     [(ima-cdr-attach) (blank 0 car-point-y)]
                     [(body-with-attach) (vl-append ima-car-attach
                                                    (ht-append ima-cdr-attach
                                                               cell-body))]
                     [(cell-x-inset) (max 0 (- attach-x car-point-x))]
                     [(cell-y-inset) (max 0 (- attach-y car-point-y))]
                     [(car-x-inset) (max 0 (- car-point-x attach-x))]
                     [(cdr-y-inset) (max 0 (- car-point-y attach-y))]
                     [(body-and-car) (vl-append vertical-spacing
                                                (inset
                                                 body-with-attach
                                                 cell-x-inset cell-y-inset 0 0)
                                                (inset
                                                 car-drawn
                                                 car-x-inset 0 0 0))]
                     [(body-no-arrows) (ht-append horizontal-spacing
                                                  body-and-car
                                                  (inset
                                                   cdr-drawn
                                                   0 cdr-y-inset 0 0))]
                     [(picture) (pin-arrow-line
                                 arrow-head-size
                                 (pin-arrow-line
                                  arrow-head-size
                                  body-no-arrows
                                  car-part cc-find
                                  car-attach rb-find
                                  #:line-width arrow-thickness
                                  #:color arrow-color)
                                 cdr-part cc-find
                                 cdr-attach rb-find
                                 #:line-width arrow-thickness
                                 #:color arrow-color)])
         (values picture ima-car-attach ima-cdr-attach))]
      [(pict-convertible? obj)
       (let* ([padded (inset obj object-padding)]
              [car-attach (blank (/ (pict-width padded) 2) 0)]
              [cdr-attach (blank 0 (/ (pict-height padded) 2))])
         (values (vl-append car-attach
                            (ht-append cdr-attach padded))
                 car-attach cdr-attach))]
      [else (rec (scale (typeset-code (datum->syntax #f obj))
                        text-scale)
                 0 0)]))
  (let-values ([(pict _1 _2) (rec (if (syntax? obj)
                                      (syntax->datum obj)
                                      obj)
                                  0 0)])
    pict))
