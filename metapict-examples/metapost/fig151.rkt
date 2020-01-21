#lang racket

(require metapict
         racket/math
         "../common.rkt")

(set-curve-pict-size 450 450)

(define (disc-at d r)
  (let ([P (pt* 2 (pt (+ (sind (* 2 d)) (cos d)) (cosd (* 3 d))))])
        (circle P r)))

(define fig151
  (with-window (window -3.2 3.2 -3.2 3.2)
    (for/draw ([i (in-range 0 360)])
      ;; clip area is slightly larger than disc to factor in border
      (clipped (rectangle (pt -10 -10) (pt 10 10))
               (curve-reverse (disc-at (+ i 1) 0.21))
               (brushcolor "white" (filldraw (disc-at i 0.2)))))))

fig151

(save-svg "fig151.svg" fig151)
