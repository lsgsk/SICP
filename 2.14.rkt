#lang racket
(define (make-interval a b) (cons a b))
(define (make-center-percent c p) (make-interval (* c (- 1 (/ p 100))) (* c (+ 1 (/ p 100)))))
(define (lower-bound x)(car x))
(define (upper-bound x)(cdr x))


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

(define (div-interval x y)
  (if (<= (* (upper-bound y) (lower-bound y)) 0)
      (error "Incorrect division" y)
      (mul-interval x
                    (make-interval (/ 1.0 (upper-bound y))
                                   (/ 1.0 (lower-bound y))))))


(define (par1 r1 r2)
  (div-interval (mul-interval r1 r2)
                (add-interval r1 r2)))

(define (par2 r1 r2)
  (let ((one (make-interval 1 1)))
    (div-interval one
          (add-interval (div-interval one r1)
                        (div-interval one r2)))))


(define int (make-center-percent 1.0 5))
(define int2 (make-center-percent 5.0 5))
int
int2
(div-interval int int)
(par1 int int2)
(par2 int int2)



