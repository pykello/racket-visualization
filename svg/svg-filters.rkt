#lang racket

(require xml
         pict
         pict/shadow
         racket/draw
         racket/dict
         racket/match)


(define (feFlood color opacity w h)
  (define color-with-alpha
    (make-object color%
      (send color red)
      (send color green)
      (send color blue)
      opacity))
  (filled-rectangle w h
                    #:draw-border? #f
                    #:color color-with-alpha))

(define (feOffset p dx dy)
  (let ([w (+ (pict-width p) (abs dx))]
        [h (+ (pict-height p) (abs dy))]
        [x (if (> dx 0)
               dx
               0)]
        [y (if (> dy 0)
               dy
               0)])
    (pin-over (blank w h)
              x y
              p)))

(define (feComposite operator p1 p2)
  (define w1 (pict-width p1))
  (define h1 (pict-height p1))
  (define w2 (pict-width p2))
  (define h2 (pict-height p2))
  (define w (max w1 w2))
  (define h (max h1 h2))
  (define bmp1 (send (pict->bitmap p1) make-dc))
  (define bmp2 (send (pict->bitmap p2) make-dc))
  (define result-bmp (make-object bitmap% w h #f #t))
  (define bmp-dc (send result-bmp make-dc))
  
  (for* ([x (in-range w)]
         [y (in-range h)])
    (let* ([c1 (make-object color%)]
           [c2 (make-object color%)]
           [r1 (send bmp1 get-pixel x y c1)]
           [r2 (send bmp2 get-pixel x y c2)]
           [c1-exists? (and (< x w1) (< y h1))]
           [c2-exists? (and (< x w2) (< y w2))]
           [rc1 (if c1-exists? c1 (make-object color% 0 0 0 0))]
           [rc2 (if c2-exists? c2 (make-object color% 0 0 0 0))])
      ;;(displayln (format "x=~a, y=~a, c1=~a, c2=~a" x y (color->string rc1) (color->string rc2)))
      (define c (composite-color operator rc1 rc2))
      ;;(displayln (format "compose=~a" (color->string c)))
      (send bmp-dc set-pixel x y c)
      ))

  (dc (λ (dc x y)
        (define-values (sx sy) (send dc get-scale))
        (send dc set-scale 1.0 1.0)
        (send dc draw-bitmap result-bmp (* x sx) (* y sy))
        (send dc set-scale sx sy))
      w h))

(define (composite-color operator c1 c2)
  (define (over-alpha a1 a2)
    (+ a1 (* (- 1 a1) a2)))
  (define (over c1 c2 a1 a2)
    (define c (+ (* c1 a1) (* c2 (- 1 a1) a2)))
    (define a (over-alpha a1 a2))
    (min 255
         (if (> a 0)
             (exact-round (/ c a))
             (exact-round c))))
  (let ([r1 (send c1 red)]
        [g1 (send c1 green)]
        [b1 (send c1 blue)]
        [a1 (send c1 alpha)]
        [r2 (send c2 red)]
        [g2 (send c2 green)]
        [b2 (send c2 blue)]
        [a2 (send c2 alpha)])
    (match operator
      ['in (if (and (> a2 0.01) (> a1 0.01))
               (make-object color% r1 g1 b1 a1)
               (make-object color% 0 0 0 0))]
      ['over (make-object color%
               (over r1 r2 a1 a2)
               (over g1 g2 a1 a2)
               (over b1 b2 a1 a2)
               (over-alpha a1 a2))])))

(define (color->string c)
  (format "(~a,~a,~a,~a)"
          (send c red)
          (send c green)
          (send c blue)
          (send c alpha)))

(define (idris-shadow p)
  (define ff current-inexact-milliseconds)
  (define s (ff))
  (define flood (feFlood (make-object color% 0 0 0 1) 0.5 200 200))
  (displayln (- (ff) s))
  (define c1 (feComposite 'in flood p))
  (displayln (- (ff) s))
  (define b (blur c1 3))
  (displayln (- (ff) s))
  (define o (feOffset b 6 6))
  (displayln (- (ff) s))
  (define c2 (feComposite 'over p o))
  (displayln (- (ff) s))
  c2)

(define alpha 1)
(define w 30)
(define shift (/ w 2))

(define (test)
  (define r1 (filled-rectangle w w #:draw-border? #f #:color (make-object color% 0 255 0 alpha)))
  ;(define r2 (feOffset (filled-rectangle w w #:draw-border? #f #:color (make-object color% 255 0 0 alpha)) shift 0))
  ;r1
  ;r2
  ;(feComposite 'in r1 r2)
  ;(feComposite 'over r1 r2)
  (idris-shadow r1))

(test)

