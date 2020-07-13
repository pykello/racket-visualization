#lang racket

(require pict
         "../metapict-examples/common.rkt")

(define w 500)

(define (rotating-squares-pict n)
  (cond
    [(= n 1) (rectangle w w)]
    [else (define subproblem (rotating-squares-pict (- n 1)))
          (define theta (atan (/ 1. 4)))
          (define rotated-subproblem
            (scale-to-fit (rotate subproblem theta) w w))
          (cc-superimpose (rectangle w w)
                          rotated-subproblem)]))

(define (rotate-pict p α β n)
  (define γ (+ α (* n (- β α))))
  (rotate p γ))

(define (rotate-pict-seq p α β frames)
  (for/list ([frame (in-range 0 frames)])
    (define t (fast-start (/ frame frames)))
    (rotate-pict p α β t)))

(define (scale-pict p s n)
  (define cur (+ 1.0 (* n (- s 1.0))))
  (scale p cur))

(define (scale-pict-seq p s frames)
  (for/list ([frame (in-range 0 frames)])
    (define t (fast-start (/ frame frames)))
    (scale-pict p s t)))

(define (fade-seq p1 p2 frames)
  (for/list ([frame (in-range 0 frames)])
    (define t (fast-end (/ frame frames)))
    (fade-pict t p1 p2)))

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

(define (output-frames seq prefix)
  (for ([i (in-range 0 (length seq))]
        [p seq])
    (define num (~a i #:width 6 #:pad-string "0" #:align 'right))
    (define name (string-append prefix num ".png"))
    (save-png name p)))

(define (create-animation seq filename framerate)
  (system "rm /tmp/frame-*.png")
  (output-frames seq "/tmp/frame-")
  (define command
    (format "ffmpeg -y -framerate ~a -i /tmp/frame-%06d.png ~a" framerate filename))
  (displayln command)
  (system command))

;;
;; creates frames for transforming n squares to n+1 squares
;;
(define (rotating-squares-anim n)
  (define start_1 (rotating-squares-pict n))
  (define seq_1 (rotate-pict-seq start_1
                                 0
                                 0.4
                                 15))

  (define start_2 (last seq_1))
  (define s (/ w (pict-width start_2)))
  (define seq_2 (scale-pict-seq start_2 s 15))

  (define start_3 (last seq_2))
  (define seq_3 (repeat start_3 8))

  (define start_4 start_3)
  (define end_4 (cc-superimpose (rectangle w w) (last seq_3)))
  (define seq_4 (fade-seq start_4 end_4 15))

  (define seq_5 (repeat end_4 15))

  (cc-superimpose-seq (blank (* w 1.5) (* w 1.5)) (append seq_1 seq_2 seq_3 seq_4 seq_5)))

(define anim
  (flatten (for/list ([i (in-range 1 11)])
             (define slides (rotating-squares-anim i))
             (define header (inset (text (format "n = ~a ⟶ n = ~a" i (+ i 1)) null 48) 0 20))
             (define header-slides (repeat header (length slides)))
             (vc-append-seq header-slides slides))))

(create-animation anim "rotating-squares.mp4" 30)
