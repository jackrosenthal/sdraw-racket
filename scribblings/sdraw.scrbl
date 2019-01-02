#lang scribble/manual
@(require racket/sandbox scribble/example sdraw
          (for-label sdraw racket pict pict/color pict/convert pict/code
                     pict/face))
@(define eval (make-base-eval))
@(eval '(require pict))
@(eval '(require sdraw))
@(eval '(require pict/code))

@title{Sdraw: Cons-Cell Diagrams with Pict}
@author[(author+email "Jack Rosenthal" "jack@rosenth.al")]

@defmodule[sdraw]

@;@examples[#:eval evaluator
@;          (sdraw '(1 2 3))]

@deftech{Cons-cell diagrams} (also called @deftech{box-and-pointer
diagrams}) are often used to visually depict
@seclink["pairs"
  #:doc '(lib "scribblings/reference/reference.scrbl")]{pairs and lists},
typically in an educational context. For example,a cons-cell diagram
for the list @racket['(1 (2 3) 4)] can be seen below:

@centerline[(sdraw '(1 (2 3) 4))]

Sdraw can be used to automatically create these diagrams using
@seclink["top" #:doc '(lib "pict/scribblings/pict.scrbl")]{Pict}.
Inspiration for sdraw comes from
@hyperlink["http://www.cs.cmu.edu/~dst/Lisp/sdraw/"]{the @tt{SDRAW} program}
included with David S. Touretzky's
@hyperlink["http://www.cs.cmu.edu/~dst/LispBook/index.html"]{
  @italic{Common Lisp: A Gentle Introduction to Symbolic Computation}}.
Like Touretzky's program, circular lists and other graph-like
structures are supported.

@section{Usage}

@(define datum
   (tech #:doc '(lib "scribblings/reference/reference.scrbl") "datum"))
@(define syntax
   (tech #:doc '(lib "scribblings/reference/reference.scrbl") "syntax object"))

There is only one function provided by sdraw:

@defproc[(sdraw [obj any/c]
                [#:cell-border-color cell-border-color color/c "Black"]
                [#:cell-inside-color cell-inside-color color/c "White"]
                [#:arrow-color arrow-color color/c "Black"]
                [#:arrow-thickness arrow-thickness real? 2.5]
                [#:arrow-head-size arrow-head-size real? 7.5]
                [#:arrow-point-size arrow-point-size real? 7.5]
                [#:object-padding object-padding real? 1]
                [#:cell-inside-size cell-inside-size real? 15]
                [#:cell-inside-radius cell-inside-radius real? 1.5]
                [#:cell-border-radius cell-border-radius real? 1.5]
                [#:cell-border-width cell-border-width real? 2]
                [#:vertical-spacing vertical-spacing real? 25]
                [#:horizontal-spacing horizontal-spacing real? 40]
                [#:etc-pict etc-pict pict-convertible? (text "etc.")]
                [#:null-style null-style (or/c pict-convertible? '/ '|\| 'x)
                              (typeset-code #'())]
                [#:null-thickness null-thickness real? 2.5]
                [#:max-depth max-depth (or/c +inf.0 natural-number/c) +inf.0]
                [#:max-width max-width (or/c +inf.0 natural-number/c) +inf.0]
                [#:reference-label reference-label
                                   (-> string? pict-convertible?)
                                   default-reference-label])
            pict?]{
    Draws a cons-cell diagram of @racket[obj]. @racket[obj] can either be a
    @datum or a @elem[syntax]. If a @syntax is given, it will be
    converted to a @datum using @racket[syntax->dautm].

    Pairs are drawn as a box, with arrows going down and right for the
    @racket[car] and @racket[cdr] respectively. If a @racket[pict-convertible?]
    is given, it will be drawn directly. Otherwise, it will be converted to
    a @racket[pict?] using @racket[typeset-code].

    Various optional keyword arguments can be given to @racket[sdraw]
    to customize the appearance:

    @itemlist[
        @item{@bold{Limiting size:} For large objects, you may wish to
          limit the amount of pairs drawn. Using @racket[max-depth] and
          @racket[max-width], you can specify how far deep or out will
          be drawn before pairs are replaced by @racket[etc-pict].

          @examples[#:eval eval
            (sdraw (expand '(case a [(b c) #t] [else #f]))
                   #:max-width 3
                   #:max-depth 2)]}

        @item{@bold{Null style:} By default, null will be represented
          by an arrow to @racket[()]. However, other styles are common
          practice.  For example, in
          @hyperlink["https://mitpress.mit.edu/sicp/"]{SICP}, null is
          represented using a slash thru the containing pair.

          @racket[null-style] may specify an alternative
          @racket[pict-convertible?] to show. For example:

          @examples[#:eval eval
            (sdraw '(nconc result) #:null-style (text "NIL"))
            (sdraw '(()) #:null-style (typeset-code #'null))]

          Alternatively, @racket[null-style] may specify one of three
          in-cell styles:

          @examples[#:eval eval
            (sdraw '(()) #:null-style '/)
            (sdraw '(()) #:null-style 'x)
            (sdraw '(()) #:null-style '|\|)]

          Finally, use the @racket[null-thickness] option to adjust
          the width of the lines used to draw in-cell styles (the
          option has no effect when @racket[null-style] is
          @racket[pict-convertible?]):

          @examples[#:eval eval
            (sdraw '(1 2) #:null-style '/ #:null-thickness 0.5)]}

        @item{@bold{Spacing:} Use the @racket[horizontal-spacing] and
          @racket[vertical-spacing] options.

          @examples[#:eval eval
            (sdraw '(a b) #:horizontal-spacing 100
                          #:vertical-spacing 10)]
          To add space around the text and any other drawn pictures,
          use the @racket[object-padding] option:

          @examples[#:eval eval
            (require pict/face)
            (define happy (scale (face 'happy) 0.2))
            (sdraw `(1 ,happy (,happy . ,happy) . 2) #:object-padding 15)]}

        @item{@bold{Cell style:} Use the @racket[cell-border-color],
          @racket[cell-inside-color], @racket[cell-inside-size],
          @racket[cell-inside-radius], @racket[cell-border-radius], and
          @racket[cell-border-width] options.

          @examples[#:eval eval
            (sdraw '(a b) #:cell-inside-color "Green"
                          #:cell-border-color "Purple"
                          #:cell-inside-size 20
                          #:cell-inside-radius 10
                          #:cell-border-radius 10
                          #:cell-border-width 3
                          #:null-style '/)]}

        @item{@bold{Arrow style:} @racket[arrow-color],
          @racket[arrow-thickness], @racket[arrow-head-size], and
          @racket[arrow-point-size] can be used to adjust the size
          of the arrows.

          @examples[#:eval eval
            (sdraw '(+ (* 2 3) 4) #:arrow-color "Purple"
                                  #:arrow-thickness 1
                                  #:arrow-head-size 5
                                  #:arrow-point-size 10)]}]}
