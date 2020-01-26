#lang racket

(define (filtered-accumulate combiner null-value term a next b filter)
  (define (isfilted x)
    (if(filter x)
       x
       null-value))
  (if (> a b)
      null-value
      (combiner (term (isfilted a))
                (filtered-accumulate combiner null-value term (next a) next b filter))))

(define (sum-of-primes-squares start end)
  (define (square x) (* x x))
  (define (identity x) x)
  (define (inc x) (+ 1 x))
  (define (prime? n)    
    (define (smallest-divisor n)
      (find-divisor n 2))
    (define (find-divisor n test-divisor)
      (cond ((> (square test-divisor) n) n)
            ((divides? test-divisor n) test-divisor)
            (else (find-divisor n (+ test-divisor 1)))))
    (define (divides? a b)
      (= (remainder b a) 0))
    (= n (smallest-divisor n)))
  (filtered-accumulate + 0 square start inc end prime?))

(sum-of-primes-squares 1 10)

(define (product-of-simple n)
  (define (gcd a b)
    (if (= b 0)
        a
        (gcd b (remainder a b))))
  (define (identity x) x)
  (define (inc x) (+ 1 x))
  (define (even? x) (= 1 (gcd x n)))
  (filtered-accumulate * 1 identity 1 inc n even?))

(product-of-simple 10)