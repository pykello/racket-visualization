#lang racket

(require metapict
         racket/math
         slideshow/latex)

(provide (all-defined-out))

(latex-debug? #f)

;; Added to every tex file, before the document proper:
(add-preamble #<<latex
\usepackage{amsmath, amssymb}
\newcommand{\targetlang}{\lambda_{\mathrm{ZFC}}}
\newcommand{\meaningof}[1]{[\![{#1}]\!]}  % use in semantic functions
\newcommand{\A}{\mathcal{A}}
\renewcommand{\P}{\mathbb{P}}
\newcommand{\N}{\mathbb{N}}
\newcommand{\Z}{\mathbb{Z}}
\newcommand{\Q}{\mathbb{Q}}
\newcommand{\R}{\mathbb{R}}
latex
              )
(define ($ x)
           (scale 0.6 ($$ x)))

(ahlength (px 5))

(define-syntax dashed-curve
  (syntax-rules ()
    [(dashed-curve a ...) (dashed (draw (curve a ...)))]))

(define (dot-node pos fill [size 4])
  (circle-node #:at pos #:min-size (px size) #:fill fill))

(define (save-svg filename p)
  (when (file-exists? filename)
    (delete-file filename))
  (save-pict filename p 'svg))

(define (draw-rec cs)
  (if (list? cs)
      (draw* (map draw-rec cs))
      (draw cs)))

(define current-curly-brace-indent (make-parameter 0.5))
(define (curly-brace p1 p2 [a (current-curly-brace-indent)])
  (define (hcurve p1 va vb)
    (curve p1 ..
           (pt+ p1 (vec* 0.15 va) (vec* 0.5 vb)) ..
           (pt+ p1 (vec* 0.6 va) (vec* 0.98 vb)) ..
           (pt+ p1 va vb)))
  (define v1 (pt- p2 p1))
  (define vn (vec* (/ a (norm v1)) v1))
  (define vp (rot90 vn))
  (define vx (vec* 0.5 vn))
  (define -vx (vec- (vec 0 0) vx))
  (define vy (vec* 0.5 vp))
  (define -vy (vec- (vec 0 0) vy))
  (define p3 (pt+ (med 0.5 p1 p2) (vec* 2 vy)))
  (curve-append
   (hcurve p1 vx vy)
   (curve (pt+ p1 vx vy) -- (pt+ p3 -vx -vy))
   (curve-reverse (hcurve p3 -vx -vy))
   (hcurve p3 vx -vy)
   (curve (pt+ p3 (vec+ vx -vy)) -- (pt+ p2 -vx vy))
   (curve-reverse (hcurve p2 -vx vy))))

