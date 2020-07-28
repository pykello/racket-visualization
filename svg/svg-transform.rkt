#lang racket

(require xml
         racket/dict
         rsvg)

(define (transform-svg in transformations)
  (let* ([input-xml (string->xexpr
                     (port->string in))]
         [output-xml (transform-svg-xml input-xml transformations)]
         [output-str (xexpr->string output-xml)])
    (open-input-string output-str)))

(define (transform-svg-xml root txs [tx-applied? #f])
  (define (transform-child e)
    (if (list? e)
        (transform-svg-xml e txs #f)
        e))
  (define id (get-attr root 'id #f))
  (define tx (dict-ref txs id `()))
  (cond
    [(or tx-applied? (null? tx))
     (append (take root 2)
             (map transform-child (drop root 2)))]
    [else
     (displayln (format "id=~a, tx=~a" id tx))
     (list 'g (tx->svg-attrs tx)
           (transform-svg-xml root txs #t))]))

(define (tx->svg-attrs tx)
  (define opacity-attr
    (let [(opacity (dict-ref tx 'opacity 1))]
      (list 'opacity (number->string opacity))))
  (list opacity-attr))

(define (get-attr elem name default)
  (define attrs (second elem))
  (match (findf (λ(a) (equal? (first a) name)) attrs)
    [(list key value) value]
    [_ default]))

(define txs
  #hash[("blue-piece" . {(opacity . 0.1)})])

(define idris-logo-in (open-input-file "racket-logo.svg"))
(define idris-logo-out (transform-svg idris-logo-in txs))

(svg-port->pict idris-logo-out)
