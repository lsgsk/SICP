#lang racket
(define (make-interval a b) (cons a b))
(define (upper-bound x)(cdr x))
(define (lower-bound x)(car x))

(define (add-interval x y)
  (make-interval(+ (lower-bound x) (lower-bound y))
                (+ (upper-bound x) (upper-bound y))))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define (width interval)
  (/ (- (upper-bound interval)(lower-bound interval)) 2))

(width (add-interval (make-interval 1 3) (make-interval 2 4)))
(width (add-interval (make-interval 1 3) (make-interval 6 8)))

(width (mul-interval (make-interval 1 3) (make-interval 2 4)))
(width (mul-interval (make-interval 1 3) (make-interval 6 8)))
