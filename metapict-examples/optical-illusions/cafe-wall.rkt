#lang racket

(require metapict
         racket/math
         "../common.rkt")

;; https://en.wikipedia.org/wiki/Caf%C3%A9_wall_illusion

(define (delta-for-row r)
  (match (remainder r 4)
    [0 -0.5]
    [1 -0.25]
    [2 0]
    [3 -0.25]))

(define (color-for-cell r c)
  (if (odd? c)
      "black"
      "white"))

(set-curve-pict-size 560 420)

(define cafe-wall
  (with-window (window 0 10 0 9)
    (draw
     (color "gray" (filldraw (rectangle (pt 0 0) (pt 100 100))))
     (for*/draw ([row (in-range 9)]
                [col (in-range 11)])
       (let ([dx (delta-for-row row)]
             [c (color-for-cell row col)])
         (color c (filldraw (rectangle (pt (+ dx col) row) (pt (+ dx col 0.9) (+ row 0.9))))))))))

cafe-wall

(save-svg "cafe-wall.svg" cafe-wall)
