#lang racket

(require metapict
         racket/math
         "../common.rkt"
         "components.rkt")

(current-component-size 1.2)

(define r1 (make-resistor (pt 0 0) 'right))
(define r2 (make-resistor (pt 3 -1.5) 'down))
(define c1 (make-capacitor (pt -1 -1) 'up #:type 'polarized))
(define c2 (make-capacitor (pt 0 -2.5) 'left))
(define tr1 (make-transistor (pt 2 0) #:type 'pnp))

(set-curve-pict-size 400 400)
(with-window (window -4 5 -4 5)
  (draw (color "gray" (grid (pt -10 -10) (pt 10 10) (pt -10 -10) #:step 1))
        r1 r2 c2 c1 tr1
        (wire (pin r1 'out) (pin tr1 'base))
        (wire (pin r2 'out) (pin c2 'in))
        (wire (pin c2 'out) (pin c1 'in))
        (wire (pin c1 'out) (pin r1 'in))
        (wire (pin tr1 'emitter) (pin r2 'in))
        (label-for r1 ($ "12\\Omega"))
        (label-for r2 "10Ω")
        (label-for c2 "10nF")
        (label-for c1 ($ "4700{\\mu}F"))))
