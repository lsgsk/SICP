#lang racket
(define (fixed-point f first-guess)
  (define tolerance 0.00000001)
  (define (close-enough? v1 v2)
    ( < (abs (-  v1 v2)) tolerance))
  (define (try guess count)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          (begin
            (display "Steps: ")
            (display count)
            (display " Result: ")            
            next)
          (begin
            (display next)
            (newline)
            (try next (+ 1 count)))
          )))
  (try first-guess 0))

;(fixed-point (lambda (x) (+ 1 (/ 1 x))) 1.0)
;(fixed-point (lambda (x) (/(log 1000) (log x))) 5.0)
(fixed-point (lambda (x) (/ (+ x (/ (log 1000) (log x))) 2)) 5.0); интересно девки пляшут
