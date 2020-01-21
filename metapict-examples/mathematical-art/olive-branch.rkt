#lang racket

(require metapict
         racket/math
         "../common.rkt")

;; http://www.ams.org/publicoutreach/math-imagery/yeganeh
;; takes quite some time to finish

(define (A k)
  (+ (* k (/ 2 4000))
     (/ (sin (* pi k (/ 42 4000))) 28)
     (/ (expt (sin (* pi k (/ 21 4000))) 8) 9)
     (* (/ (expt (sin (* pi k (/ 21 4000))) 6) 4)
        (sin (* pi (/ 2 5) (expt (/ k 4000) 12))))))

(define (B k)
  (+ (* 0.25 (expt (/ k 4000) 2))
     (* 0.25
        (+ (expt (sin (* pi k (/ 21 4000))) 5)
           (* (/ 1 28) (sin (* pi k (/ 42 4000)))))
        (cos (* (/ pi 2) (expt (/ k 4000) 12))))))

(define (R k)
  (+ (/ 1 170)
     (* (/ 1 67)
        (expt (sin (* pi k (/ 42 4000))) 2)
        (- 1 (expt (cos (* pi k (/ 21 4000))) 4)))))

(set-curve-pict-size 2100 900)

(define olive-branch
  (with-window (window -0.5 3 -0.5 1)
    (color "DarkOliveGreen"
           (draw 
            (for/draw ([k (in-range 1 4001 1)])
              (circle (pt (A k) (B k)) (R k)))))))

olive-branch

(save-svg "olive-branch.svg" olive-branch)

