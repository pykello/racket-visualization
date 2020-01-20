#lang racket

(require metapict
         racket/math
         "../common.rkt")

(set-curve-pict-size 250 250)

(define fig133
  (let* ([left (for/list ([i (in-range 0 4)])
                 (pt 0 (+ -0.3 (* i 0.2))))]
         [right (for/list ([i (in-range 0 5)])
                  (pt 1 (+ -0.4 (* i 0.2))))]
         [vs (for/list ([p (append left right)])
               (dot-node p "black" 0.01))]
         [es (for*/list ([a left] [b right])
               (curve a -- b))])
    (with-window (window -0.2 1.2 -0.7 0.7)
      (draw* (append vs es)))))

fig133

(save-svg "fig133.svg" fig133)
