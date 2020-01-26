#lang racket
;Точки
(define (make-point x y)(cons x y))
(define (x-point p)(car p))
(define (y-point p)(cdr p))
(define (print-point p)
  (newline)
  (display "(")
  (display(x-point p))
  (display ",")
  (display(y-point p))
  (display ")"))
;Отрезки
(define (make-segment p1 p2)(cons p1 p2))
(define (start-segment segment) (car segment))
(define (end-segment segment) (cdr segment))
(define (midpoint-segment segment)
  (let ((p1 (start-segment segment))
        (p2 (end-segment segment)))
    (make-point (+ (x-point p1) (/ ( - (x-point p2) (x-point p1))  2)) (+ (y-point p1) (/ ( - (y-point p2) (y-point p1)) 2)))))
    
(define my-segment (make-segment (make-point 1 2) (make-point 4 3)))
(print-point (midpoint-segment my-segment))