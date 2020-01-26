#lang racket
(define (double g)
  (lambda (x) (g(g x))))

(define (inc x)
  (+ 1 x))

((double inc) 5)

(((double (double double)) inc) 5)


(((double double) inc) 5)

