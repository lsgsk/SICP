#lang racket

(define (make-interval a b) (cons a b))
(define (upper-bound x)(cdr x))
(define (lower-bound x)(car x))

(define (add-interval x y)
  (make-interval(+ (lower-bound x) (lower-bound y))
                (+ (upper-bound x) (upper-bound y))))

(add-interval (make-interval 1 3 ) (make-interval 2 4))