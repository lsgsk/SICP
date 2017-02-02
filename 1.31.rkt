#lang racket
(define (product term a next b)
  (if (> a b)
      1
      (* (term a)
         (product term (next a) next b))))

(define (fuctorial a b)
  (define (identity x) x)
  (define (inc x) (+ 1 x))
  (product identity a inc b))

(fuctorial 1 5)