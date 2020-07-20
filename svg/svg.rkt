#lang racket

(require xml
         pict
         racket/draw
         racket/dict
         racket/match
         (only-in metapict
                  bez
                  pt
                  bez->dc-path))

(define (draw-svg root)
  (define width (parse-dimension (get-attr root 'width "100px")))
  (define height (parse-dimension (get-attr root 'height "100px")))
  (dc (λ (dc dx dy)
         ;; save dc state
        (define smoothing (send dc get-smoothing))
        (define transformation (send dc get-transformation))
        (define pen (send dc get-pen))

        (define-values
          (xx xy yx yy x0 y0)
          (vector->values (send dc get-initial-matrix)))

        ;; defaults
        (send dc set-smoothing `smoothed)
        (send dc set-initial-matrix (vector xx xy yx yy
                                            (+ 1 x0 (* dx xx) (* dy xy))
                                            (+ 1 y0 (* dx yx) (* dy yy))))

        (send dc set-pen (new pen%
                            [color "black"]
                            [style `transparent]
                            [width 1.25]
                            [cap `butt]
                            [join `round]
                            [stipple #f]))

        (for ([elem (drop root 2)])
          (when (list? elem)
              (draw-svg-elem dc elem)))

        ;; restore dc state
        (send dc set-transformation transformation)
        (send dc set-smoothing smoothing)
        (send dc set-pen pen))
      width height))

(define (draw-svg-elem dc elem)
  (define brush (send dc get-brush))
  (define pen (send dc get-pen))

  (define elem-fill (get-attr elem 'fill #f))
  (when (string? elem-fill)
    (define color (string->color elem-fill))
    (send dc set-brush color 'solid))

  (define tag (first elem))
  (match tag
    ['circle
     (let ([cx (string->number (get-attr elem 'cx "0"))]
           [cy (string->number (get-attr elem 'cy "0"))]
           [r (string->number (get-attr elem 'r "0"))])
       (send dc draw-ellipse (- cx r) (- cy r) (* 2 r) (* 2 r)))]
    ['path
     (let* ([d (tokenize-path (get-attr elem 'd ""))]
            [path (svg-path->dc-path (new dc-path%) d 0 0)])
       (send dc draw-path path))]
    [_ 0])

  (send dc set-brush brush)
  (send dc set-pen pen))

(define (svg-path->dc-path path d cx cy)
  (match d
    [(list "M" x y rest ...)
     (svg-path->dc-path path rest x y)]

    [(list "m" dx dy rest ...)
     (svg-path->dc-path path rest (+ cx dx) (+ cy dy))]

    [(list "L" x y rest ...)
     (send path line-to x y)
     (svg-path->dc-path path rest x y)]

    [(list "l" dx dy rest ...)
     (let ([x (+ cx dx)]
           [y (+ cy dy)])
       (send path line-to x y)
       (svg-path->dc-path path rest x y))]

    [(list "C" x1 y1 x2 y2 x y rest ...)
     (let ([bez-path (bez->dc-path
                      (bez (pt cx cy)
                           (pt x1 y1)
                           (pt x2 y2)
                           (pt x y)))])
       (send path append bez-path)
       (svg-path->dc-path path rest x y))]

    [(list "c" dx1 dy1 dx2 dy2 dx dy rest ...)
     (let* ([x1 (+ dx1 cx)]
            [y1 (+ dy1 cy)]
            [x2 (+ dx2 cx)]
            [y2 (+ dy2 cy)]
            [x (+ dx cx)]
            [y (+ dy cy)]
            [bez-path (bez->dc-path
                       (bez (pt cx cy)
                            (pt x1 y1)
                            (pt x2 y2)
                            (pt x y)))])
       (send path append bez-path)
       (svg-path->dc-path path rest x y))]

    [(list "z" rest ...)
     (send path close)
     path]))

(define (get-attr elem name default)
  (define attrs (second elem))
  (match (findf (λ(a) (equal? (first a) name)) attrs)
    [(list key value) value]
    [_ default]))

(define (tokenize-path s)
  (define a (string-replace s "-" " -"))
  (define b (string-replace a "," " "))
  (define c (regexp-replace* #rx"([a-zA-Z])" b " \\1 "))
  (define tokens (string-split c))
  (define result
    (map (λ (token)
           (if (regexp-match? #rx"[0-9]" token)
               (string->number token)
               token))
         tokens))
  result)

(define (string->color s)
  (define r (substring s 1 3))
  (define g (substring s 3 5))
  (define b (substring s 5 7))
  (make-object color%
    (string->number r 16)
    (string->number g 16)
    (string->number b 16)))

(define (parse-dimension s)
  (string->number (string-replace s "px" "")))

(define racket-logo-text (string->xexpr (port->string (open-input-file "racket-logo.svg"))))
(draw-svg racket-logo-text)