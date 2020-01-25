#lang racket

(require metapict
         racket/math
         "../common.rkt"
         "components.rkt")

(current-component-size 1)

(define r1 (make-resistor (pt 6.3 7.6) 'down))
(define tr1 (make-transistor (pt 6 6)))
(define j1 (pt 6.3 5))
(define c1 (make-capacitor (pt 6.8 4.5) 'down))
(define r2 (make-resistor (pt 5.8 4.5) 'up))
(define r3 (make-resistor (pt 4 5.1) 'up))
(define r4 (make-resistor (pt 4 7.3) 'up))
(define j2 (pt 5.5 8.1))
(define c2 (make-capacitor (pt 3.2 6) 'right))
(define out (pt 7 7))
(define j3 (pt 6.3 7))
(define in (pin c2 'in))
(define j4 (pt 5 8.1))
(define vcc (pt+ j4 (vec 0 0.4)))
(define j5 (pt 5 4))
(define ground (make-ground (pt+ j5 (vec 0 -0.4))))

(set-curve-pict-size 560 420)
(define single-stage-amplifier
  (with-window (window 1 9 3 9)
    (draw r1 tr1 c1 r2 r3 r4 c2 ground
          (filldraw (circle out 0.05))
          (filldraw (circle in 0.05))
          (filldraw (circle vcc 0.05))
          (label-for r1 "4.7kΩ")
          (label-for c1 "20μF")
          (label-for r2 "1kΩ")
          (label-for r3 "1kΩ")
          (label-for r4 "4.7kΩ")
          (label-for c2 "1μF")
          (label-for ground "0V")
          (wire (pin r1 'out) (pin tr1 'collector))
          (wire j1 (pin tr1 'emitter))
          (wire j1 (pin c1 'in))
          (wire (pin c1 'out) (pin r2 'in))
          (wire (pin r2 'out) j1)
          (wire (pin r2 'in) (pin r3 'in))
          (wire (pin r3 'out) (pin r4 'in))
          (wire (pin r4 'out) j2)
          (wire j2 (pin r1 'in))
          (wire (pin c2 'out) (pin tr1 'base))
          (wire j3 out)
          (wire j4 vcc)
          (wire j5 (pin ground 'in))
          (label-rt "Output Signal" (pt+ out (vec 0.1 0)))
          (label-lft "Input Signal" (pt- in (vec 0.1 0)))
          (label-top "9V" (pt+ vcc (vec 0 0.1)))
          )))

single-stage-amplifier

(save-svg "single-stage-amplifier.svg" single-stage-amplifier)