#lang racket

(require metapict
         racket/math
         "../common.rkt")

(set-curve-pict-size 250 250)

(define fig132
  (let* ([ps (for/list ([θ (in-range 0 (* 2 pi) (/ (* 2 pi) 7))])
               (pt@ 1 θ))]
         [vs (for/list ([p ps])
               (dot-node p "black"))]
         [es (for*/list ([p1 ps] [p2 ps]
                                 #:unless (eq? p1 p2))
               (curve p1 -- p2))])
    (with-window (window -1.2 1.2 -1.2 1.2)
      (draw* (append vs es)))))

fig132

(save-svg "fig132.svg" fig132)

