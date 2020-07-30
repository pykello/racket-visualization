#lang racket

(require "common.rkt"
         pict
         racket/dict
         "../svg/svg-transform.rkt"
         rsvg)

(define (svg-animate time svg-in anims)
  (define txs
    (make-hash
     (for/list ([element-anim anims])
       (element-anim->tx time element-anim))))
  (transform-svg svg-in txs))

(define (element-anim->tx time element-anim)
  (define element-name (car element-anim))
  (define property-anims (cdr element-anim))
  (cons element-name
   (for/list ([property-anim property-anims])
     (property-anim->tx time property-anim))))

(define (property-anim->tx time property-anim)
  (define property-defaults
    '([opacity 1.0]
      [translate (0 0)]))

  (define property (car property-anim))
  (define keyframes
    (let* ([default (dict-ref property-defaults property)]
           [a (cdr property-anim)]
           ;; add a sentinel first key-frame
           [b (cons (cons 0 default) a)]
           [lst (last b)]
           ;; add a sentinel last key-frame
           [c (append b (list (cons 100000 (cdr lst))))])
      c))

  (define-values (keyframe-a keyframe-b)
    (find-interval keyframes time))

  (define time-a (car keyframe-a))
  (define time-b (car keyframe-b))
  (define value-a (cdr keyframe-a))
  (define value-b (cdr keyframe-b))

  (define t (fast-start (/ (- time time-a) (- time-b time-a))))

  (define (interpolate t a b)
    (cond
      [(number? a)  (* 1.0 (+ a (* t (- b a))))]
      [(list? a) (for/list ([sub-a a]
                            [sub-b b])
                   (interpolate t sub-a sub-b))]))
  (define property-value (interpolate t value-a value-b))
  (define result
    (match property
      ['opacity (cons property property-value)]
      ['translate (cons 'transform (list (list 'translate
                                               (* 1.0 (first property-value))
                                               (* 1.0 (second property-value)))))]))
  result)

(define (find-interval keyframes time)
  (define a (first keyframes))
  (define b (second keyframes))
  (if (and (>= time (car a))
           (< time (car b)))
      (values a b)
      (find-interval (cdr keyframes) time)))

(define (generate-frame time)
  (define svg-in (open-input-file "tree.svg"))

  (define level2-anim
    '([opacity . {(0 . 0) (1 . 0) (2 . 1) (3 . 1) (4 . 0)}]
      [translate . {(0 . [-100 0]) (5 . [100 0])}]))

  (define svg-out
    (svg-animate time svg-in
                 (list (cons "level2" level2-anim))))

  (svg-port->pict svg-out))

(define fps 24)
(define frames (generate-frames generate-frame 5 fps))
frames
(create-animation frames `() "svg-animation-01.mp4" fps)
