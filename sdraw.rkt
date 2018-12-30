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

(require racket/list)
(require racket/contract)
(require racket/function)
(require pict)
(require pict/code)
(require pict/convert)
(require pict/color)
(provide sdraw)

(define-syntax sequence
  (syntax-rules ()
    [(_ name body0 body ...)
     (let ([name (let ([result body0])
                   (if (void? result)
                       name
                       result))])
       (sequence name body ...))]
    [(_ name)
     name]))

(define default-etc-pict (text "etc."))
(define default-null-pict (typeset-code #'()))

(define/contract (sdraw obj
                        #:cell-border-color [cell-border-color "Black"]
                        #:cell-inside-color [cell-inside-color "White"]
                        #:arrow-color [arrow-color "Black"]
                        #:arrow-thickness [arrow-thickness 2.5]
                        #:arrow-head-size [arrow-head-size 7.5]
                        #:arrow-point-size [arrow-point-size 7.5]
                        #:object-padding [object-padding 1]
                        #:cell-inside-size [cell-inside-size 15]
                        #:cell-inside-radius [cell-inside-radius 1.5]
                        #:cell-border-radius [cell-border-radius 1.5]
                        #:cell-border-width [cell-border-width 2]
                        #:vertical-spacing [vertical-spacing 25]
                        #:horizontal-spacing [horizontal-spacing 40]
                        #:etc-pict [etc-pict default-etc-pict]
                        #:null-style [null-style default-null-pict]
                        #:null-thickness [null-thickness 2.5]
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
        #:cell-inside-size real?
        #:cell-inside-radius real?
        #:cell-border-radius real?
        #:cell-border-width real?
        #:vertical-spacing real?
        #:horizontal-spacing real?
        #:etc-pict pict-convertible?
        #:null-style (or/c pict-convertible? '/ '|\| 'x)
        #:null-thickness real?
        #:max-depth (or/c +inf.0 natural-number/c)
        #:max-width (or/c +inf.0 natural-number/c))
       pict?)

  (define (cons-part)
    (filled-rounded-rectangle cell-inside-size cell-inside-size
                              cell-inside-radius
                              #:color cell-inside-color
                              #:draw-border? #f))

  (define inset-fcns
    (cons (λ (pict value) (inset pict (max 0 value) 0 0 0))
          (λ (pict value) (inset pict 0 (max 0 value) 0 0))))

  (define (arrow pict from to)
    (sequence pict
              (pin-over pict
                        from
                        lt-find
                        (cc-superimpose (blank (pict-width from)
                                               (pict-height from))
                                        (filled-ellipse arrow-point-size
                                                        arrow-point-size
                                                        #:color arrow-color
                                                        #:draw-border? #f)))
              (pin-arrow-line arrow-head-size
                              pict
                              from cc-find
                              to rb-find
                              #:line-width arrow-thickness
                              #:color arrow-color)))

  (define attach-box-tbl (make-hasheq))

  ;; add blank boxes to a pict which can be used to determine where to attach
  ;; arrows for the car/cdr
  (define (make-attach-boxes! pict right down)
    (let* ([car-box (blank right 0)]
           [cdr-box (blank 0 down)]
           [consed-boxes (cons car-box cdr-box)]
           [with-boxes (vl-append car-box (ht-append cdr-box pict))])
      (hash-set! attach-box-tbl pict consed-boxes)
      (hash-set! attach-box-tbl with-boxes consed-boxes)
      with-boxes))

  (define (attach-boxes pict)
    (hash-ref attach-box-tbl pict))

  (define (attach-box-offset pict attach-pict)
    (let-values ([(x y) (rb-find pict attach-pict)])
      (max x y)))

  (define spaced-append-fcns
    (cons (curry vl-append vertical-spacing)
          (curry ht-append horizontal-spacing)))

  (define (rec obj depth width)
    (cond
      [(null? obj)
       ;; in the case of (sdraw '() #:null-style '/), or similar
       (rec (if (pict-convertible? null-style)
                null-style
                default-null-pict)
            0 0)]
      [(pair? obj)
       (if (or (> depth max-depth)
               (> width max-width))
           ;; show etc-pict in the case of maxed width/depth
           (rec etc-pict 0 0)

           (let* ([parts (cons (cons-part) (cons-part))]
                  [cell-inner (hc-append cell-border-width
                                         (car parts) (cdr parts))]
                  [cell-body (cc-superimpose
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
                  [part-centers (let-values
                                    ([(x y) (cc-find cell-body (car parts))])
                                  (cons x y))])

             (define (add-from-rec base side)
               (if (and (not (pict-convertible? null-style))
                        (null? (side obj)))
                   (let ([part (side parts)])
                     (sequence base
                               (when (member null-style '(/ x))
                                 (pin-line base
                                           part lb-find
                                           part rt-find
                                           #:line-width null-thickness))
                               (when (member null-style '(|\| x))
                                 (pin-line base
                                           part lt-find
                                           part rb-find
                                           #:line-width null-thickness))))
                   (let* ([pict (rec (side obj)
                                     (side (cons (add1 depth) depth))
                                     (side (cons width (add1 width))))]
                          [attach-box (side (attach-boxes pict))]
                          [attach-offset (attach-box-offset pict attach-box)]
                          [spaced-append (side spaced-append-fcns)]
                          [part-center (side part-centers)]
                          [inset (side inset-fcns)])
                     (arrow
                      ;; the pict that the arrows go on
                      (spaced-append
                       (inset base (- attach-offset part-center))
                       (inset pict (- part-center attach-offset)))
                      ;; from
                      (side parts)
                      ;; to
                      attach-box))))

             (sequence pict
                       (make-attach-boxes! cell-body
                                           (car part-centers)
                                           (cdr part-centers))
                       (add-from-rec pict car)
                       (add-from-rec pict cdr)
                       (hash-set! attach-box-tbl pict
                                  (hash-ref attach-box-tbl cell-body)))))]
      [(pict-convertible? obj)
       (sequence pict
                 (inset obj object-padding)
                 (make-attach-boxes! pict
                                     (/ (pict-width pict) 2)
                                     (/ (pict-height pict) 2)))]
      [else (rec (typeset-code (datum->syntax #f obj)) 0 0)]))
  (rec (if (syntax? obj)
           (syntax->datum obj)
           obj) 0 0))
