#lang racket

(define (product term a next b)
  (if (> a b)
      1
      (* (term a)
         (product term (next a) next b))))

(define (product-iter term a next b)
  (define (iter a result)
    (if(> a b)
       result
       (iter (next a) (* (term a) result))))
  (iter a 1))

(define (fuctorial a b)
  (define (identity x) x)
  (define (inc x) (+ 1 x))
  (product-iter identity a inc b))

(fuctorial 1 5)

(define (pi n)
  (define (square x) (* x x))
  (define (term x) (/ (* x (+ x 2)) (square(+ x 1))))    
  (define (next x) (+ x 2))
  (* 4 (product term 2.0 next n)))

(pi 10000000)