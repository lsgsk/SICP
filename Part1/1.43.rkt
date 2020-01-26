#lang racket

(define (inc x) (+ 1 x))
(define (square x) (* x x))

(define (repeated g n)
  (define (rep x m)
    (if (= m 0)        
         x       
        (g (rep x (- m 1)))))
  (lambda (x) (rep x n)))


((repeated square 2) 5)
((repeated square 3) 5)
((repeated inc 5) 5)

