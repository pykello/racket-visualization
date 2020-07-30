#lang racket

(require pict
         (only-in rsound
                  rs-append*
                  rs-write
                  silence)
         "../metapict-examples/common.rkt")

(provide (all-defined-out))

(define (output-frames seq prefix)
  (for/async ([i (in-range 0 (length seq))]
              [p seq])
    (define num (~a i #:width 6 #:pad-string "0" #:align 'right))
    (define name (string-append prefix num ".bmp"))
    (displayln (format "outputting ~a (total=~a) ..." name (length seq)))
    (save-bmp name p)))


(define (create-animation frame-seq audio-seq output-filename framerate)
  (system "rm -rf /tmp/frame-*.png")
  (system "rm -rf /tmp/frame-*.bmp")
  (output-frames frame-seq "/tmp/frame-")
  (define audio-combined (if (null? audio-seq)
                                    (silence 1000)
                                    (rs-append* audio-seq)))
  (define audio-filename "/tmp/racket-audio.wav")
  (rs-write audio-combined audio-filename)
  (define command
    (format "ffmpeg -y -framerate ~a -i /tmp/frame-%06d.bmp -i ~a -vcodec libx264  -pix_fmt yuv420p ~a" framerate audio-filename output-filename))
  (displayln command)
  (system command))

(define (visible-in from to current-time p)
  (define w (pict-width p))
  (define h (pict-height p))
  (define b (blank w h))
  (define fade-duration 0.75)
  (cond
    [(<= from current-time (+ from fade-duration))
     (define t (min 1.0 (/ (- current-time from) fade-duration)))
     (fade-pict (fast-end t) b p)]
    [(<= (- to fade-duration) current-time to)
     (define t (max 0.0 (/ (- to current-time) fade-duration)))
     (fade-pict (fast-end t) b p)]
    [(<= from current-time to) p]
    [else b]) )

(define (generate-frames generate-frame len fps)
  (define frame-count (ceiling (* len fps)))
  (for/list [(i (in-range frame-count))]
    (define t (/ i fps))
    (generate-frame t)))

