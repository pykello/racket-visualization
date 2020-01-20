#lang racket

(require metapict
         racket/math
         "../common.rkt")

(set-curve-pict-size 400 400)

(define fig85
  (with-window (window -0.2 1.2 -0.2 1.2)
    (draw*
     (for/fold ([A (pt 0 0)]
                [B (pt 1 0)]
                [C (pt 1 1)]
                [D (pt 0 1)]
                [acc '()]
                #:result acc)
               ([i (in-range 1 101)])
       (values (med 0.2 A B) (med 0.2 B C) (med 0.2 C D) (med 0.2 D A)
               (cons (curve A -- B -- C -- D -- A) acc))))))

fig85

(save-svg "fig85.svg" fig85)
