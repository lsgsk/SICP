#lang racket
(define (avarage a b)
  (/ (+ a b) 2))
(define (avarage-damp f) 
  (lambda (x) (avarage x (f x))))

(define (fixed-point f first-guess)
  (define tolerance 0.0000000000000001)
  (define (close-enough? v1 v2)
    ( < (abs (-  v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(define (repeated f n)
  (define (recursion x m)
    (if (= m 0)
        x
        (f (recursion x (- m 1)))))
  (lambda (x) (recursion x n)))


(define (sqrt1 x n)
  (fixed-point
   ((repeated (lambda (g) (avarage-damp g)) n) (lambda (y) (/ x y)))   
   1.0))
(define (sqrt2 x)
  (fixed-point
   (lambda (y) (avarage y (/ x y)))
   1.0))
(define (sqrt3 x n)
  (fixed-point
   ((repeated (lambda (g) (avarage-damp g)) n) (lambda (y) (/ x (* y y y))))  ; ебтвоюмать
   1.0))

(sqrt1 4 2)
(sqrt2 4)
(sqrt3 81 2)