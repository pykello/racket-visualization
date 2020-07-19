#lang racket

(require pict
         (only-in rsound
                  rs-read
                  rs-frame-rate
                  rs-frames
                  silence
                  rs-append
                  rs-append*
                  rs-write)
         "common.rkt"
         future-visualizer)

(define w 500)
(define theta (atan (/ 1. 4)))
(define border 3)

(define (rotating-squares-pict n)
  (cond
    [(= n 1) (rectangle w w #:border-width border)]
    [else (define subproblem (rotating-squares-pict (- n 1)))
          (define rotated-subproblem
            (scale-to-fit (rotate subproblem theta) w w))
          (cc-superimpose (rectangle w w #:border-width border)
                          rotated-subproblem)]))

(define (rotate-pict p α β n)
  (define γ (+ α (* n (- β α))))
  (rotate p γ))

(define (rotate-pict-seq p α β frames)
  (for/list ([frame (in-range 0 (+ 1 frames))])
    (define t (fast-start (/ frame frames)))
    (rotate-pict p α β t)))

(define (scale-pict p s n)
  (define cur (+ 1.0 (* n (- s 1.0))))
  (scale p cur))

(define (scale-pict-seq p s frames)
  (for/list ([frame (in-range 0 (+ 1 frames))])
    (define t (fast-start (/ frame frames)))
    (scale-pict p s t)))

(define (repeat p n)
  (for/list ([i (in-range n)])
    p))

(define (cc-superimpose-seq base seq)
  (for/list ([p seq])
    (cc-superimpose base p)))

(define (vc-append-seq seq1 seq2)
  (for/list ([p1 seq1]
             [p2 seq2])
    (vc-append p1 p2)))

;;
;; creates frames for transforming n squares to n+1 squares
;;
(define (rotating-squares-anim start_1 n [fps 24] [pause #t])
  (define half (ceiling (/ fps 2)))
  (define one fps)
  (define quarter (ceiling (/ fps 4)))
  (define seq_1 (rotate-pict-seq start_1
                                 0
                                 theta
                                 half))

  (define start_2 (last seq_1))
  (define w2 (pict-width (rotating-squares-pict (+ n 1))))
  (define s (/ (- w2 2) (pict-width start_2)))
  (define seq_2 (scale-pict-seq start_2 s half))

  (define start_3 (last seq_2))
  (define seq_3 (repeat start_3 quarter))

  (define start_4 start_3)
  (define end_4 (cc-superimpose (rectangle w w #:border-width border) (last seq_3)))

  (define seq_5 (repeat end_4 one))

  (if pause
      (append seq_1 seq_2 seq_3 seq_5)
      (append seq_1 seq_2 (list end_4))))

(define fps 24)
(define (anim fps [pause #t] [n 21] [speeder 0])
  (define tick (rs-read "tick.wav"))
  (for/fold ([start_1 (rotating-squares-pict 1)]
             [all `()]
             [fps-real fps]
             [sounds `()]
             #:result (values all sounds))
            ([i (in-range 1 n)])
    (define fps2 (ceiling fps-real))
    (define slides (rotating-squares-anim start_1 i fps2 pause))
    (define header (inset (text (format "n = ~a ⟶ n = ~a" i (+ i 1)) null 48) 0 20))
    (define header-slides (repeat header (length slides)))
    (define sound-frames (ceiling (/ (* (rs-frame-rate tick) (length slides)) fps)))
    (define sound (rs-append tick (silence (- sound-frames (rs-frames tick)))))
    (define layedout
      (vc-append-seq header-slides
                     (cc-superimpose-seq (blank (* w 1.5) (* w 1.5)) slides)))
       
    (define next-start (last slides))
    (define new-fps (max 8 (- fps speeder)))
    (define new-fps-real (max 8 (/ fps-real speeder)))
    (values next-start (append all layedout) new-fps-real
            (append sounds (list sound)))))
             
(define-values (frame-seq audio-seq) (anim 24 #f 50 1.045))
(create-animation frame-seq audio-seq "rotating-squares-super-accelerated-with-audio.mp4" fps)
