#lang racket
(define (even? n) (= (remainder n 2) 0))

(define (square  x) (* x x))

(define (fast-expt b n)
  (fast-iter-expt 1 b n))


(define (fast-iter-expt a b n)
  (cond ((= n 0) a)
        ((even? n) (fast-iter-expt a (square b) (/ n 2)))
        (else (fast-iter-expt (* a b) b (- n 1)))))



(fast-expt 2 11)