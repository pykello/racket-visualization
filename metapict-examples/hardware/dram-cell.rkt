#lang racket

(require metapict
         racket/math
         "../common.rkt"
         "components.rkt")


(define tr1 (make-transistor (pt 4 4) 'down #:type 'nmos #:label "MOSFET"))
(define cs (make-capacitor (pt 5.5 3) 'down #:label "Capacitive node"))
(define g (make-ground (pt 5.5 2) #:label "GND"))
(define j1 (pt 3 3.75))
(define j2 (pt 4 5))

(set-curve-pict-size 450 300)
(define dram-cell
  (with-window (window 0 9 1 7)
    (draw
     (color "gray" (grid (pt 0 0) (pt 10 10) (pt 0 0) #:step 1))
     tr1 cs g
     (wire (pin tr1 'drain) (pin cs 'in))
     (wire (pin cs 'out) (pin g 'in))
     (wire (pt 2 5) (pt 6 5))
     (wire (pt 3 6) (pt 3 2))
     (wire j1 (pin tr1 'source))
     (wire j2 (pin tr1 'gate))
     (junction j1)
     (junction j2)
     (label-rt "Bit Line" (pt 3.1 6))
     (label-top "Word Line" (pt 1.5 5)))))

dram-cell

(save-svg "dram-cell.svg" dram-cell)

