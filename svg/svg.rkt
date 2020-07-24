#lang racket

(require xml
         pict
         racket/draw
         racket/dict
         racket/match)

(define (draw-svg root)
  (define width (parse-dimension (get-attr root 'width "100px")))
  (define height (parse-dimension (get-attr root 'height "100px")))
  (dc (λ (dc dx dy)
        (define state (save-state dc))

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
                              [style 'transparent]
                              [width 1.25]
                              [cap `butt]
                              [join `round]
                              [stipple #f]))

        (for ([elem (drop root 2)])
          (when (list? elem)
            (draw-svg-elem dc elem)))

        (restore-state dc state))
      width height))

(define (draw-svg-elem dc elem)
  (define state (save-state dc))
  (apply-common-attributes dc elem)

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
    ['g
     (for ([e (drop elem 2)])
       (when (list? e)
         (draw-svg-elem dc e)))]

    ['text
     (render-text-elem dc elem)]

    [_ 0])

  (restore-state dc state))

;;
;; svg-path->dc-path
;;
(define (svg-path->dc-path path d cx cy [prev ""])

  (define (add-curve x1 y1 x2 y2 x y rest)
    (send path curve-to x1 y1 x2 y2 x y)
    (svg-path->dc-path path rest x y "C"))

  (define (add-curve-delta dx1 dy1 dx2 dy2 dx dy rest)
    (let* ([x1 (+ dx1 cx)]
            [y1 (+ dy1 cy)]
            [x2 (+ dx2 cx)]
            [y2 (+ dy2 cy)]
            [x (+ dx cx)]
            [y (+ dy cy)])
       (send path curve-to x1 y1 x2 y2 x y)
       (svg-path->dc-path path rest x y "c")))
    
  (match d
    [(list "M" x y rest ...)
     (send path move-to x y)
     (svg-path->dc-path path rest x y "M")]

    [(list "m" dx dy rest ...)
     (send path move-to (+ cx dx) (+ cy dy))
     (svg-path->dc-path path rest (+ cx dx) (+ cy dy) "m")]

    [(list "L" x y rest ...)
     (send path line-to x y)
     (svg-path->dc-path path rest x y "L")]

    [(list "l" dx dy rest ...)
     (let ([x (+ cx dx)]
           [y (+ cy dy)])
       (send path line-to x y)
       (svg-path->dc-path path rest x y "l"))]

    [(list "C" x1 y1 x2 y2 x y rest ...)
     (add-curve x1 y1 x2 y2 x y rest)]

    [(list (? number? x1) y1 x2 y2 x y rest ...)
     #:when (eq? prev "C")
     (add-curve x1 y1 x2 y2 x y rest)]

    [(list "c" dx1 dy1 dx2 dy2 dx dy rest ...)
     (add-curve-delta dx1 dy1 dx2 dy2 dx dy rest)]

    [(list (? number? dx1) dy1 dx2 dy2 dx dy rest ...)
     #:when (eq? prev "c")
     (add-curve-delta dx1 dy1 dx2 dy2 dx dy rest)]

    [(list "z" rest ...)
     (send path close)
     path]))

;;
;; render-text-elem
;;
(define (render-text-elem dc elem [x 0] [y 0])
  (define elem-x (get-attr-number elem 'x x))
  (define elem-y (get-attr-number elem 'y y))

  (define next-x
    (for/fold ([cur-x elem-x])
              ([e (drop elem 2)])
      (cond
        [(and (list? e)
              (eq? (first e) 'tspan))
         (render-text-elem dc e cur-x elem-y)]
        [(string? e)
         (draw-text dc e cur-x elem-y)]
        [else
         (error "unexpected element inside <text>" e)])))
  next-x)

;;
;; draw-text
;;
(define (draw-text dc s x y)
  (define-values (w h c d)
    (send dc get-text-extent s))
  (send dc draw-text s x (+ (- y h) c))
  (+ x w))

(define (get-attr elem name default)
  (define attrs (second elem))
  (match (findf (λ(a) (equal? (first a) name)) attrs)
    [(list key value) value]
    [_ default]))

(define (get-attr-number elem name default)
  (string->number (get-attr elem name (number->string default))))

(define (tokenize-path s)
  (define a (string-replace s "-" " -"))
  (define b (string-replace a "," " "))
  (define c (regexp-replace* #rx"([a-zA-Z])" b " \\1 "))
  (define tokens (string-split c))
  (num-tokens->numbers tokens))

(define (num-tokens->numbers tokens)
  (map (λ (token)
         (if (regexp-match? #rx"[0-9]" token)
             (string->number token)
             token))
       tokens))

;;
;; apply-common-attributes
;;
(define (apply-common-attributes dc elem)
  (define elem-fill (get-attr elem 'fill #f))
  (when (string? elem-fill)
    (define color (string->color elem-fill))
    (send dc set-brush color 'solid))

  (define style (get-attr elem 'style #f))
  (when (string? style)
    (apply-style dc style))

  (define transform (get-attr elem 'transform #f))
  (when (string? transform)
    (apply-transform dc transform)))

;;
;; apply-style
;;
(define (apply-style dc s)

  ;; fill
  (define fill (get-value s "fill" #f))
  (when (string? fill)
    (define color (string->color fill))
    (send dc set-brush color 'solid))

  ;; font
  (let* ([current-font (send dc get-font)]
         [size (get-value-number s "font-size"
                                  (send current-font get-size))]
         [face (get-value s "font-family"
                           (send current-font get-face))]
         [face2 "TeX Gyre Pagella, Normal"]
         [family (send current-font get-family)])
    (define font (make-object font% size face2 'system))
    (send dc set-font font)))

;;
;; apply-transform
;;
(define (apply-transform dc s)
  (define (recurse tokens)
    (match tokens
      [(list "matrix" a b c d e f rest ...)
       (send dc transform (vector a b c d e f))
       (recurse rest)]
      [(list "scale" (? number? x) (? number? y) rest ...)
       (send dc scale x y)
       (recurse rest)]
      [(list "scale" (? number? x) rest ...)
       (send dc scale x x)
       (recurse rest)]
      [(list "translate" (? number? x) (? number? y) rest ...)
       (send dc translate x y)
       (recurse rest)]
      [(list "translate" (? number? x) rest ...)
       (send dc translate x 0)
       (recurse rest)]
      [`() 0]))
  (recurse (tokenize-transform s)))

(define (tokenize-transform s)
  (define a (regexp-replace* #rx"[(),]" s " "))
  (define tokens (string-split a))
  (num-tokens->numbers tokens))

(define (get-value s key default)
  (define a (regexp-replace* #rx"[:;]" s " "))
  (define tokens (string-split a))
  (define (worker rem)
    (cond
      [(< (length rem) 2) default]
      [(equal? (first rem) key) (second rem)]
      [else (worker (cddr rem))]))
  (worker tokens))

(define (get-value-number s key default)
  (define a (get-value s key (number->string default)))
  (define b (regexp-replace* #rx"[a-zA-Z]" a ""))
  (string->number b))

(define (string->color s)
  (define r (substring s 1 3))
  (define g (substring s 3 5))
  (define b (substring s 5 7))
  (make-object color%
    (string->number r 16)
    (string->number g 16)
    (string->number b 16)))

(define (save-state dc)
  (list (send dc get-brush)
        (send dc get-pen)
        (send dc get-transformation)
        (send dc get-font)
        (send dc get-smoothing)))

(define (restore-state dc st)
  (send dc set-brush (first st))
  (send dc set-pen (second st))
  (send dc set-transformation (third st))
  (send dc set-font (fourth st))
  (send dc set-smoothing (fifth st)))

(define (parse-dimension s)
  (string->number (string-replace s "px" "")))

(define racket-logo-text (string->xexpr (port->string (open-input-file "idris-logo.svg"))))
(draw-svg racket-logo-text)
