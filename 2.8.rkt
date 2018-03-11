#lang racket

(define (make-interval a b) (cons a b))
(define (upper-bound x)(cdr x))
(define (lower-bound x)(car x))

(define (sub-interval x y)
  (make-interval(- (lower-bound x) (upper-bound y))
                (- (upper-bound x) (lower-bound y))))


(sub-interval (make-interval 5 8 ) (make-interval 2 4))
(sub-interval (make-interval 2 6 ) (make-interval 0 3))