#lang racket
(define (even? n) (= (remainder n 2) 0))
(define (double  x) (* 2 x))
(define (halve  x) (/ x 2))

(define (fast-mult x y)
  (define (fast-iter-mult a b s)
    (cond (( = b 0) s)
          ((even? b) (fast-iter-mult (double a) (halve b) s))
          (else (fast-iter-mult a (- b 1) (+ s a)))))
  (fast-iter-mult x y 0))
                     
(fast-mult 21 13)