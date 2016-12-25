#lang racket
(define (even? n) (= (remainder n 2) 0))
(define (double  x) (* 2 x))
(define (halve  x) (/ x 2))

(define (fast-mult a b)
    (cond ((= b 0) 0)
          (( = b 1) a)
          ((even? b) (double (fast-mult a (halve b))))
          (else (+ a (fast-mult a (- b 1))))))
                     
(fast-mult 21 13)