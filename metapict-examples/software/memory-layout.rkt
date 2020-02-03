#lang racket

(require metapict
         racket/math
         "../common.rkt")

;; Similar to https://tex.stackexchange.com/questions/497431/drawing-a-memory-layout-diagram-with-tikz

(define (memory-layout items)
  (current-curly-brace-indent 1.5)

  ;; draw a bit
  (define (draw-bit x0 color)
    (brushcolor color (filldraw
                       (rectangle (pt x0 0) (pt (+ x0 1) 2)))))

  ;; draws n bit cells starting from x0 with the given color
  (define (draw-bits n x0 color)
    (for/draw ([i (in-range n)])
      (draw-bit (+ x0 i) color)))

  ;; draws memory segment described by v=(length color label)
  (define (draw-item v x0)
    (let* ([n (first v)]
           [color (second v)]
           [label (third v)]
           [x1 (+ x0 n)])
      (list
       (draw-bits n x0 color)
       (curve (pt x1 0) -- (pt x1 3))
       (label-top (~v x1) (pt x1 3))
       (if label
           (curly-brace (pt x1 -0.2) (pt x0 -0.2) label)
           `()))))

  ;; how to draw a memory layout?
  (draw-rec
   (for/fold ([x0 0]
              [acc (list (label-top "0" (pt 0 3))
                         (curve (pt 0 0) -- (pt 0 3)))]
              #:result acc)
             ([item items])
     (values (+ x0 (car item))
             (cons (draw-item item x0) acc)))))

(set-curve-pict-size 640 100)
(define mem-layout
  (with-window (window 63 -1 -5 5)
    (font-size 10
               (memory-layout `((12 "LightBlue" "A")
                                (9 "Moccasin" "B")
                                (9 "LightPink" "C")
                                (9 "Khaki" "D")
                                (9 "PaleGreen" "E")
                                (14 "white" #f))))))

mem-layout

(save-svg "memory-layout.svg" mem-layout)
