#lang racket
(define (make-interval a b) (cons a b))
(define (make-center-percent c p) (make-interval (* c (- 1 (/ p 100))) (* c (+ 1 (/ p 100)))))


(define (lower-bound x)(car x))
(define (upper-bound x)(cdr x))
(define (center i) (/ (+ (upper-bound i) (lower-bound i)) 2))
(define (percent i)(display (/ (* 100 (/ (- (upper-bound i) (lower-bound i)) 2)) (center i)))(display "%"))

(define (add-interval x y)
  (make-interval(+ (lower-bound x) (lower-bound y))
                (+ (upper-bound x) (upper-bound y))))


(define interval (make-center-percent 30 10))
(center interval)
(percent interval)
