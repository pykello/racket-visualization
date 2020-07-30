#lang racket

(require xml
         racket/dict
         rsvg)

(provide transform-svg)

(define (transform-svg in transformations)
  (let* ([input-xml (string->xexpr
                     (port->string in))]
         [output-xml (transform-svg-xml input-xml transformations)]
         [output-str (xexpr->string output-xml)])
    (open-input-string output-str)))

(define (transform-svg-xml root txs)
  (define (transform-child e)
      (if (list? e)
          (worker e #f)
          e))

  (define (worker root tx-applied?)    
    (define id (get-attr root 'id #f))
    (define tx (dict-ref txs id `()))
    (cond
      [(or tx-applied? (null? tx))
       (append (take root 2)
               (map transform-child (drop root 2)))]
      [else
       (list 'g (tx->svg-attrs tx)
             (worker root #t))]))

  (append (take root 2)
          (list '(rect ((width "10000") (height "10000") (fill "white"))))
          (map transform-child (drop root 2))))

(define (tx->svg-attrs tx)
  (define opacity-attr
    (let [(opacity (dict-ref tx 'opacity 1))]
      (list 'opacity (number->string opacity))))

  (define transforms
    (for/list ([t (dict-ref tx 'transform '())])
      (format "~a(~a)"
              (car t)
              (string-join (map number->string (cdr t)) " "))))
  (define transform-attr
    (list 'transform (string-join transforms " ")))

  (list opacity-attr transform-attr))

(define (get-attr elem name default)
  (define attrs (second elem))
  (match (findf (λ(a) (equal? (first a) name)) attrs)
    [(list key value) value]
    [_ default]))

(define (test)
  (define txs
    #hash[("blue-piece" . {(opacity . 0.9)
                           (transform . [(translate 0 0)])})])
  (define idris-logo-in (open-input-file "racket-logo.svg"))
  (define idris-logo-out (transform-svg idris-logo-in txs))

  (svg-port->pict idris-logo-out))
