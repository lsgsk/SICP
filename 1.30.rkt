#lang racket
(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(define (sum-integers a b)
  (define (identity x) x)
  (define (inc x) (+ 1 x))
  (sum identity a inc b))

(sum-integers 1 10)

(define (sum-iter term a next b)
  (define (iter a result)
    (if(> a b)
       result
       (iter (next a) (+ (term a) result))))
  (iter a 0))
  

(define (sum-iter-integers a b)
  (define (identity x) x)
  (define (inc x) (+ 1 x))
  (sum-iter identity a inc b))

(sum-iter-integers 1 10)