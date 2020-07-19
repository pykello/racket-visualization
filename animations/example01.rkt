#lang racket

(require "common.rkt"
         pict)

(define (generate-frame time)
  (cc-superimpose
   (filled-rectangle 300 300 #:draw-border? #f #:color "white")
   (visible-in 1 4 time
               (filled-ellipse 100 100 #:draw-border? #f))))

(define fps 30)
(define frames (generate-frames generate-frame 5 fps))
(create-animation frames `() "1.mp4" fps)
