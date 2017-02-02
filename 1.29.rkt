#lang racket
(define (cube x) (* x x x))

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(define (intergral f a b dx)
  (define (add-dx x) (+ x dx))
  (* (sum f (+ a (/ dx 2)) add-dx b) dx))

(intergral cube 0 1 0.001)
(intergral cube 0 1 0.0001)

(define (simson-integral  f a b n)
  (define h (/ (- b a) n))
  (define (yk k)(f (+ a (* k h))))  
  (define (simson-next k)  
    (cond ((= n (+ 2 k))(+ 3 k))
          (else (+ 2 k))))
  (define (func k) (+ (* 4 (yk k)) (* 2(yk (+ 1 k)))))
  (* (/ h 3) (+ (yk 0) (yk n) (sum func 1 simson-next n))))

(simson-integral cube 0.0 1 1000)
(simson-integral cube 0.0 1 10000)
