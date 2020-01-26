#lang racket

(require metapict
         racket/math
         "../common.rkt")

;; https://en.wikipedia.org/wiki/Ebbinghaus_illusion

(define (circles center d1 d2 cnt)
  (draw* (cons (color "orange" (filldraw (circle center d1)))
               (for/list ([θ (in-range 0 360 (/ 360 cnt))])
                 (define c (pt+ center (pt@d (+ d1 (* 1.5 d2)) θ)))
                 (color "SkyBlue"
                        (filldraw (circle c d2)))))))

(set-curve-pict-size 500 300)
(define ebbinghaus
  (with-window (window 0 10 0 6)
    (draw (circles (pt 3 3) 0.5 0.8 6)
          (circles (pt 7.5 3) 0.5 0.2 8))))

ebbinghaus

(save-svg "ebbinghaus.svg" ebbinghaus)
