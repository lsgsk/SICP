#lang racket
(require rnrs/base-6)
(require rnrs/mutable-pairs-6)

(define (count-pairs x)
  (if (not (pair? x))
      0
      (+ (count-pairs (car x))
         (count-pairs (cdr x))
         1)))


(define x1 '(1 2 3))
(count-pairs x1)

(define a '(1))
(define b (cons a a))
(define x2 (list b))
(count-pairs x2)

(define q '(1))
(define w (cons q q))
(define x3 (cons w w))
(count-pairs x3)

(define x4 '(1 2 3))
(set-cdr! (cddr x4) x4)
;(count-pairs x4)

(display "end")