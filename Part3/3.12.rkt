#lang racket
(require rnrs/base-6)
(require rnrs/mutable-pairs-6)

(define (last-pair x)
  (if (null? (cdr x))
      x
      (last-pair (cdr x))))

(define (append! x y)
  (set-cdr! (last-pair x) y)
  x)


(define x (list 1 2 3))
(define y (list 4 5 6))
(define z (append x y))
x
y
z
(display "----------\n")
(define w (append! x y))
w
x
y

