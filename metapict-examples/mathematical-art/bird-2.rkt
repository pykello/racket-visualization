#lang racket

(require metapict
         racket/math
         "../common.rkt")

(set-curve-pict-size 1800 600)

;; http://www.ams.org/publicoutreach/math-imagery/yeganeh

(define bird-2
  (with-window (window -3 3 -1 1)
  (for/draw ([i (in-range 1 2001 1)])
    (curve (pt (* 3 (expt (sin (* pi i 0.001)) 3))
               (- (cos (* pi i 0.004))))
           --
           (pt (* 1.5 (expt (sin (* pi i 0.001)) 3))
               (* -0.5 (cos (* pi i (/ 6 2000)))))))))

(scale 0.2 bird-2)

(save-svg "bird-2.svg" bird-2)

