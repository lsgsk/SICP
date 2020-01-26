#lang racket
(require rnrs/base-6)
(require rnrs/mutable-pairs-6)

(define (mystery x)
  (define (loop x y)
    (if (null? x)
        y
        (let ((temp (cdr x)))
          (set-cdr! x y)
          (loop temp x))))
  (loop x '()))

(mystery (list 1 2 3))