#lang racket

(require metapict
         metapict/path-operations
         racket/math)

(provide (all-defined-out))

(define current-component-size (make-parameter 1))
(define current-label-margin (make-parameter (λ() (* (current-component-size) 0.1))))

(define (current-label-margin->num)
  (if (procedure? (current-label-margin))
      ((current-label-margin))
      (current-label-margin)))

(struct component (center w h pins label-f))

(define (2-pin-w orientation thickness)
  (match orientation
    [(or 'left 'right) (current-component-size)]
    [(or 'up 'down) (* thickness (current-component-size))]))

(define (2-pin-h orientation thickness)
  (match orientation
    [(or 'up 'down) (current-component-size)]
    [(or 'left 'right) (* thickness (current-component-size))]))

(define (2-pin-pins center orientation)
  (define dx (/ (current-component-size) 2))
  (match orientation
    ['up (make-immutable-hash
          `([in . ,(pt+ center (vec* dx down))]
            [out . ,(pt+ center (vec* dx up))]))]
    ['down (make-immutable-hash
            `([in . ,(pt+ center (vec* dx up))]
              [out . ,(pt+ center (vec* dx down))]))]
    ['right (make-immutable-hash
             `([in . ,(pt+ center (vec* dx left))]
               [out . ,(pt+ center (vec* dx right))]))]
    ['left (make-immutable-hash
            `([in . ,(pt+ center (vec* dx right))]
              [out . ,(pt+ center (vec* dx left))]))]))

(define (2-pin-label-func center orientation thickness)
  (define margin
    (+ (* 0.5 thickness)
       (current-label-margin->num)))
  (match orientation
    ['right (λ (t) (label-top t (pt+ center (vec* margin up))))]
    ['left (λ (t) (label-bot t (pt+ center (vec* margin down))))]
    ['up (λ (t) (label-lft t (pt+ center (vec* margin left))))]
    ['down (λ (t) (label-rt t (pt+ center (vec* margin right))))]))

(define (resistor-thickness)
  (* (current-component-size) 0.2))

(define (capacitor-thickness)
  (* (current-component-size) 0.3))

(define (2-pin-transform comp orientation)
  (define θ (match orientation
              [(or 'up 'down) (/ pi 2)]
              [(or 'left 'right) 0]))
  (λ (c) (shifted (component-center comp)
                  (scaled (current-component-size)
                          (rotated θ c)))))


(struct capacitor component (orientation type)
  #:property prop:drawable (λ (c) (match (capacitor-type c)
                                    ['fixed (draw-fixed-capacitor c)]
                                    ['polarized (draw-polarized-capacitor c)])))

(define (make-capacitor center orientation #:type (type 'fixed))
  (define thickness (capacitor-thickness))
  (capacitor center
             (2-pin-w orientation thickness)
             (2-pin-h orientation thickness)
             (2-pin-pins center orientation)
             (2-pin-label-func center orientation thickness)
             orientation
             type))

(define (draw-fixed-capacitor c)
  (define max-y (/ (capacitor-thickness) 2))
  (define min-y (- max-y))
  (define transform (2-pin-transform c (capacitor-orientation c)))
  (draw (transform (curve (pt -0.5 0) --
                          (pt -0.075 0) --
                          (pt -0.075 max-y) --
                          (pt -0.075 min-y)))
        (transform (curve (pt 0.5 0) --
                          (pt 0.075 0) --
                          (pt 0.075 max-y) --
                          (pt 0.075 min-y)))))

(define (draw-polarized-capacitor c)
  (define max-y (/ (capacitor-thickness) 2))
  (define min-y (- max-y))
  (define transform (2-pin-transform c (capacitor-orientation c)))
  (draw (transform (curve (pt -0.5 0) --
                          (pt -0.06 0) --
                          (pt -0.06 max-y) --
                          (pt -0.06 min-y)))
        (transform (curve (pt 0.5 0) --
                          (pt 0.05 0)))
        (transform (curve (pt 0.16 max-y) ..
                          (pt 0.05 0) ..
                          (pt 0.16 min-y)))))

(struct resistor component (orientation)
  #:property prop:drawable (λ (r) (draw-resistor r)))

(define (make-resistor center orientation)
  (define thickness (resistor-thickness))
  (resistor center
            (2-pin-w orientation thickness)
            (2-pin-h orientation thickness)
            (2-pin-pins center orientation)
            (2-pin-label-func center orientation thickness)
            orientation))

(define (draw-resistor r)
  (define max-y (/ (resistor-thickness) 2))
  (define min-y (- max-y))
  (define transform (2-pin-transform r (resistor-orientation r)))
  (define base-curve (curve (pt -0.5 0) --
                            (pt -0.3 0) --
                            (pt -0.25 max-y) --
                            (pt -0.15 min-y) --
                            (pt -0.05 max-y) --
                            (pt 0.05 min-y) --
                            (pt 0.15 max-y) --
                            (pt 0.25 min-y) --
                            (pt 0.3 0) --
                            (pt 0.5 0)))
  (draw (transform base-curve)))

(struct transistor component (type orientation flipped)
  #:property prop:drawable (λ (t) (draw-transistor t)))

(define (transistor-transform center orientation flipped)
  (define θ (match orientation
              ['right 0]
              ['up (/ pi 2)]
              ['left pi]
              ['down (* 1.5 pi)]))
  (define flip-if-necessary (if flipped flipy identity))
  (λ (c) (shifted center
                  (scaled (current-component-size)
                          (rotated θ (flip-if-necessary c))))))

(define (make-transistor center [orientation 'right] [flipped #f] #:type [type 'npn])
  (let* ([transform (transistor-transform center orientation flipped)]
         [a (transform (pt -0.5 0))]
         [b (transform (pt@d 0.5 -60))]
         [c (transform (pt@d 0.5 60))]
         [margin (+ (* (current-component-size) 0.5) (current-label-margin->num))]
         [pins (make-immutable-hash`([base . ,a]
                                     [emitter . ,b]
                                     [collector . ,c]
                                     [gate . ,a]
                                     [source . ,b]
                                     [drain . ,c]))]
         [label-func (λ (t) (match orientation
                              ['right (label-rt t (pt+ center (vec* margin right)))]
                              ['left (label-lft t (pt+ center (vec* margin left)))]
                              ['top (label-top t (pt+ center (vec* margin top)))]
                              ['bottom (label-bot t (pt+ center (vec* margin down)))]))])
    (transistor center
                (current-component-size)
                (current-component-size)
                pins
                label-func
                type
                orientation
                flipped)))

(define (draw-transistor t)
  (define transform (transistor-transform (component-center t)
                                          (transistor-orientation t)
                                          (transistor-flipped t)))
  (define type (transistor-type t))
  (match type
    [(or 'npn 'pnp) (draw (transform (circle 0 0 0.5))
                          (transform (curve (pt -0.5 0) --
                                            (pt -0.2 0) --
                                            (pt -0.2 0.25) --
                                            (pt -0.2 -0.25)))
                          (transform (curve (pt -0.2 0.1) --
                                            (pt@d 0.5 60)))
                          (transform (curve (pt -0.2 -0.1) --
                                            (pt@d 0.5 -60)))
                          (match type
                            ['npn (draw-arrow (transform (curve (pt -0.2 -0.1) --
                                                                (med 0.7 (pt -0.2 -0.1) (pt@d 0.5 -60)))))]
                            ['pnp (draw-arrow (transform (curve (pt@d 0.5 -60) --
                                                                (med 0.3 (pt -0.2 -0.1) (pt@d 0.5 -60)))))]))]))

(struct ground component ()
  #:property prop:drawable (λ (g) (draw-ground g)))

(define (make-ground center)
  (define s (/ (current-component-size) 2))
  (ground center s s
          (make-immutable-hash `([in . ,(pt+ center (vec* s (vec 0 0.25)))]))
          (λ (t)
            (label-bot t (pt+ center (vec* s (vec 0 (- -0.25 (current-label-margin->num)))))))))

(define (draw-ground g)
  (define transform (2-pin-transform g 'right))
  (draw (transform (curve (pt -0.16 0) -- (pt 0.16 0)))
        (transform (curve (pt -0.08 -0.1) -- (pt 0.08 -0.1)))
        (transform (curve (pt -0.25 0.1) -- (pt 0.25 0.1)))))


(define (pin c tag)
  (hash-ref (component-pins c) tag))

(define (wire p1 p2 #:cw (cw #t))
  (define-values
    (x1 y1 x2 y2)
    (values (pt-x p1) (pt-y p1)
            (pt-x p2) (pt-y p2)))
  (cond
    [(or (and cw (> x2 x1) (< y2 y1))
         (and cw (< x2 x1) (> y2 y1))
         (and (not cw) (< x2 x1) (< y2 y1))
         (and (not cw) (> x2 x1) (> y2 y1)))
     (curve p1 -/ p2)]
    [else (curve p1 /- p2)]))

(define (label-for c t)
  ((component-label-f c) t))
