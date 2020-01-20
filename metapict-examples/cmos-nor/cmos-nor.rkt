#lang racket

(require metapict
         racket/math
         "../common.rkt")

(set-curve-pict-size 400 400)

(define cmos-nor
(with-window (window -1 12 -1 12)
  (draw (curve (pt 0 5) -- (pt 2 5))
        (curve (pt 0 8) -- (pt 6 8))
        (curve (pt 7 9) -- (pt 6 9) -- (pt 6 3) -- (pt 7 3))
        (curve (pt 3 3) -- (pt 2 3) -- (pt 2 6) -- (pt 7 6))

        (curve (pt 3.25 3.75) -- (pt 3.5 3.75) -- (pt 3.5 4.25) -- (pt 10 4.25))

        ;; Transistor left down
        (curve (pt 3 2) -- (pt 3 4))
        (dashed-curve (pt 3.25 2) -- (pt 3.25 4))
        (curve (pt 3.25 2.25) -- (pt 3.5 2.25) -- (pt 3.5 1) -- (pt 7.5 1) -- (pt 7.5 2.25) -- (pt 7.25 2.25))
        (draw-arrow (curve (pt 4 3) -- (pt 3.25 3)))

        ;; Transistor right down
        (curve (pt 7 2) -- (pt 7 4))
        (dashed-curve (pt 7.25 2) -- (pt 7.25 4))
        (draw-arrow (curve  (pt 8 3) -- (pt 7.25 3)))

        (curve (pt 7.25 3.75) -- (pt 7.5 3.75) -- (pt 7.5 5.25) -- (pt 7.25 5.25))

        ;; Transistor right middle
        (curve (pt 7 5) -- (pt 7 7))
        (dashed-curve (pt 7.25 5) -- (pt 7.25 7))
        (draw-arrow (curve (pt 7.25 6) -- (pt 8 6)))

        (curve (pt 7.25 6.75) -- (pt 7.5 6.75) -- (pt 7.5 8.25) -- (pt 7.25 8.25))

        ;; Transistor right top
        (curve (pt 7 8) -- (pt 7 10))
        (dashed-curve (pt 7.25 8) -- (pt 7.25 10))
        (curve (pt 7.25 9.75) -- (pt 7.5 9.75) -- (pt 7.5 11))
        (draw-arrow (curve (pt 7.25 9) -- (pt 8 9)))

        ;; GND
        (label-cnt "GND" (pt 8 0))
        (curve (pt 6 0) -- (pt 6 1))
        (curve (pt 5 0) -- (pt 7 0))
        (curve (pt 5.2 -0.2) -- (pt 6.8 -0.2))
        (curve (pt 5.4 -0.4) -- (pt 6.6 -0.4))

        (label-cnt ($ "U_{dd}") (pt 8.25 11))
        (label-cnt ($ "a \\overline{\\lor} b") (pt 9.5 4.6))
        (label-cnt ($ "a") (pt -0.5 8))
        (label-cnt ($ "b") (pt -0.5 5))

        (dot-node 6 8 "black")
        (dot-node 7.5 4.25 "black")
        (dot-node 0 5 "white")
        (dot-node 0 8 "white"))))

cmos-nor

(save-svg "cmos-nor.svg" cmos-nor)

