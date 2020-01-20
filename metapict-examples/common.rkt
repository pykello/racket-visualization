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

(define (dot-node x y fill)
  (circle-node #:at (pt x y) #:min-size (px 4) #:fill fill))

(define (save-svg filename p)
  (when (file-exists? filename)
    (delete-file filename))
  (save-pict filename p 'svg))
