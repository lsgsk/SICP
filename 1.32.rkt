#lang racket

(define (sum term a next b)
  (accumulate + 0 term a next b)) 
(define (product term a next b)
  (accumulate-iter * 1 term a next b))

(define (accumulate combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a)
                (accumulate combiner null-value term (next a) next b))))

(define (accumulate-iter combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (combiner result (term a)))))
    (iter a null-value))

(define (intsum b)
  (define (identity x) x)
  (define (inc x) (+ 1 x))
  (sum identity 1 inc b))

(define (fuctorial b)
  (define (identity x) x)
  (define (inc x) (+ 1 x))
  (product identity 1 inc b))

(intsum 5)
(fuctorial 5)