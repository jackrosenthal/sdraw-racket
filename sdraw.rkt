#lang racket
;; Port of SDRAW program from David S. Touretzky's
;; "Common Lisp: A Gentle Introduction to Symbolic Computation"
;;
;; This program draws cons-cell diagrams using Racket's pict library
;; Example usage: (sdraw '(1 2 (+ 3 4) 5))
;;
;; Author: Jack Rosenthal <jack@rosenth.al>
;; This program is licensed under the MIT License. You should have
;; received a file "LICENSE" with this program explaining the details.

(require pict)
(require pict/code)

(define cell-border-color (make-parameter "Black"))
(define cell-inside-color (make-parameter "White"))
(define arrow-color (make-parameter "Black"))
(define arrow-thickness (make-parameter 5))
(define arrow-head-size (make-parameter 15))
(define arrow-point-size (make-parameter 15))
(define text-padding (make-parameter 2))
(define text-scale (make-parameter 1.5))
(define cell-inside-size (make-parameter 30))
(define cell-inside-radius (make-parameter 3))
(define cell-border-radius (make-parameter 3))
(define cell-border-width (make-parameter 4))
(define vertical-spacing (make-parameter 50))
(define horizontal-spacing (make-parameter 80))

(define (cons-part)
  (cc-superimpose
   (filled-rounded-rectangle (cell-inside-size) (cell-inside-size)
                             (cell-inside-radius)
                             #:color (cell-inside-color)
                             #:draw-border? #f)
   (filled-ellipse (arrow-point-size) (arrow-point-size)
                   #:color (arrow-color)
                   #:draw-border? #f)))

;; returns (values pict car-attach cdr-attach)
(define (sdraw-rec obj)
  (if (cons? obj)
      (let*-values ([(car-part) (cons-part)]
                    [(cdr-part) (cons-part)]
                    [(cell-inner) (hc-append (cell-border-width)
                                             car-part cdr-part)]
                    [(cell-body) (cc-superimpose
                                  (scale-to-fit
                                   (filled-rounded-rectangle
                                    (pict-width cell-inner)
                                    (pict-height cell-inner)
                                    (cell-border-radius)
                                    #:draw-border? #f)
                                   (+ (pict-width cell-inner)
                                      (* 2 (cell-border-width)))
                                   (+ (pict-height cell-inner)
                                      (* 2 (cell-border-width)))
                                   #:mode 'distort)
                                  cell-inner)]
                    [(car-drawn car-attach _1) (sdraw-rec (car obj))]
                    [(cdr-drawn _2 cdr-attach) (sdraw-rec (cdr obj))]
                    [(attach-x _3) (rb-find car-drawn car-attach)]
                    [(_4 attach-y) (rb-find cdr-drawn cdr-attach)]
                    [(car-point-x car-point-y) (cc-find cell-body car-part)]
                    [(ima-car-attach) (blank car-point-x 0)]
                    [(ima-cdr-attach) (blank 0 car-point-y)]
                    [(body-with-attach) (vl-append ima-car-attach
                                                   (ht-append ima-cdr-attach
                                                              cell-body))]
                    [(cell-x-inset) (max 0 (- attach-x car-point-x))]
                    [(car-x-inset) (max 0 (- car-point-x attach-x))]
                    [(cdr-y-inset) (max 0 (- car-point-y attach-y))]
                    [(body-and-car) (vl-append (vertical-spacing)
                                               (inset
                                                body-with-attach
                                                cell-x-inset 0 0 0)
                                               (inset
                                                car-drawn
                                                car-x-inset 0 0 0))]
                    [(body-no-arrows) (ht-append (horizontal-spacing)
                                                 body-and-car
                                                 (inset
                                                  cdr-drawn
                                                  0 cdr-y-inset 0 0))]
                    [(picture) (pin-arrow-line
                                (arrow-head-size)
                                (pin-arrow-line
                                 (arrow-head-size)
                                 body-no-arrows
                                 car-part cc-find
                                 car-attach rb-find
                                 #:line-width (arrow-thickness))
                                cdr-part cc-find
                                cdr-attach rb-find
                                #:line-width (arrow-thickness))])
        (values picture ima-car-attach ima-cdr-attach))
      (let* ([code-pict (inset (scale (typeset-code (datum->syntax #f obj))
                                      (text-scale))
                               (text-padding))]
             [car-attach (blank (/ (pict-width code-pict) 2) 0)]
             [cdr-attach (blank 0 (/ (pict-height code-pict) 2))])
        (values (vl-append car-attach
                           (ht-append cdr-attach
                                      code-pict))
                car-attach cdr-attach))))

(define (sdraw obj)
  (let ([datum (if (syntax? obj)
                   (syntax->datum obj)
                   obj)])
    (let-values ([(pict _1 _2) (sdraw-rec datum)])
      pict)))
