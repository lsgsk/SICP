#lang racket

(define dx 0.000001)
(define (smooth f) (lambda (x) (/ ( + (f (- x dx)) (f x) (f (+ x dx))) 3)))
(define (square x) (* x x))
(define (repeated g n)
  (define (rep x m)
    (if (= m 0)
        x
        (g (rep x (- m 1)))))
  (lambda (x) (rep x n)))

(define (nsmooth f n)
    ((repeated (lambda (y) (smooth y)) n) f))       

((smooth square) 5)
((nsmooth square 2) 5)



