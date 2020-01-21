#lang racket

(require metapict
         racket/math
         "../common.rkt")

(define (striped-cell u dx dy)
  (for/list ([k (in-range 0 5)])
    (let ([ku (* k u 0.2)])
      (color "gray"
             (filldraw
              (shifted (pt dx dy)
                       (cond
                         [(odd? k) (curve (pt ku 0) -- (pt u (- u ku)) --
                                          (pt u (- u ku (/ u 5))) --
                                          (pt (+ ku (/ u 5)) 0) -- cycle)]
                         [else (curve (pt 0 ku) -- (pt (- u ku) u) --
                                      (pt (- u ku (/ u 5)) u) --
                                      (pt 0 (+ ku (/ u 5))) -- cycle)])))))))

(set-curve-pict-size 400 400)

(define fig195
  (with-window (window -4 44 -4 44)
    (let* ([n 8]
           [u 5]
           [striped-cells (for*/list ([i (in-range 0 n)]
                                      [j (in-range 0 n)]
                                      #:when (odd? (+ i j)))
                            (striped-cell 5 (* i u) (* j u)))])
      (draw*
       (append striped-cells
               (list
                (penwidth 1.5 (grid (pt 0 0) (pt (* n u) (* n u)) #:step u))))))))

fig195

(save-svg "fig195.svg" fig195)
