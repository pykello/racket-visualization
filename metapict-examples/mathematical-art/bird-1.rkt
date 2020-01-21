#lang racket

(require metapict
         racket/math
         "../common.rkt")

(set-curve-pict-size 480 200)

;; formula based on https://en.wikipedia.org/wiki/A_Bird_in_Flight

(define bird-1
  (with-window (window -1.5 1.5 -0.75 0.5)
  (for/draw ([i (in-range 1 501)])
    (curve (pt (* 1.5 (expt (sin (+ (/ pi 3) (* 2 pi i 0.002))) 7))
               (* 0.25 (expt (cos (* 6 pi i 0.002)) 2)))
           --
           (pt (* 0.2 (sin (+ (* 6 pi i 0.002) (/ pi 5))))
               (* (- (/ 2 3)) (expt (sin (- (* 2 pi i 0.002) (/ pi 3))) 2)))))))

bird-1

(save-svg "bird-1.svg" bird-1)

