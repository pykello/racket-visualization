#lang racket

(require metapict
         racket/math
         "../common.rkt"
         "components.rkt")

(current-component-size 1)

(define r1 (make-resistor (pt 6.3 7.6) 'down #:label "4.7kΩ"))
(define tr1 (make-transistor (pt 6 6)))
(define j1 (pt 6.3 5))
(define c1 (make-capacitor (pt 6.8 4.5) 'down #:label "20μF"))
(define r2 (make-resistor (pt 5.8 4.5) 'up #:label "1kΩ"))
(define r3 (make-resistor (pt 4 5.1) 'up #:label "1kΩ"))
(define r4 (make-resistor (pt 4 7.3) 'up #:label "4.7kΩ"))
(define j2 (pt 5.5 8.1))
(define c2 (make-capacitor (pt 3.2 6) 'right #:label "1μF"))
(define out (pt 7 7))
(define j3 (pt 6.3 7))
(define in (pin c2 'in))
(define j4 (pt 5 8.1))
(define vcc (pt+ j4 (vec 0 0.4)))
(define j5 (pt 5 4))
(define ground (make-ground (pt+ j5 (vec 0 -0.4)) #:label "0V"))

(set-curve-pict-size 560 420)
(define single-stage-amplifier
  (with-window (window 1 9 3 9)
    (draw r1 tr1 c1 r2 r3 r4 c2 ground
          (junction out)
          (junction in)
          (junction vcc)
          (wire (pin r1 'out) (pin tr1 'collector))
          (wire j1 (pin tr1 'emitter))
          (multiwire (list j1 c1 r2 j1))
          (wire (pin r2 'in) (pin r3 'in))
          (multiwire (list r3 r4 j2 r1))
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
