#lang racket

(require plot
         plot/no-gui)

;; https://en.wikipedia.org/wiki/Barnsley_fern

(define (next-point p)
  (define x (first p))
  (define y (second p))
  (define r (random))
  (cond
    [(< r 0.01) (list 0 (* 0.16 y))]
     [(< r 0.08) (list (- (* 0.2 x) (* 0.26 y))
                       (+ (* 0.23 x) (* 0.22 y) 1.6))]
     [(< r 0.15) (list (+ (* -0.15 x) (* 0.28 y))
                       (+ (* 0.26 x) (* 0.24 y) 0.44))]
     [else (list (+ (* 0.85 x) (* 0.04 y))
                 (+ (* -0.04 x) (* 0.85 y) 1.6))]))

(define barnsley-fern
  (points
       (for*/fold ([p `(0 0)]
                   [acc `()]
                   #:result acc)
                  ([i (in-range 40000)])
         (values (next-point p)
                 (cons p acc)))
       #:color "ForestGreen"
       #:size 2))

(plot barnsley-fern
      #:x-min -3 #:x-max 3)

(plot-file barnsley-fern
           "barnsley-fern.png"
           #:x-min -3 #:x-max 3)
