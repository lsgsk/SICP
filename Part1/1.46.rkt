#lang racket
(define (% a b) (* (/ b 100) a))

(define (iterative-improve good-enouth? improve)
  (define (iter guess)
    (let [(next (improve guess))]
      (if (good-enouth? guess next)
          guess
          (iter next))))
  (lambda(x) (iter x)))

(define (sqrt x)  
  (define (improve guess)
    (/ (+ guess (/ x guess)) 2)) 
  (define (good-enouth? prev next)
    (< (abs (- next prev)) (% 0.0001 prev)))
  ((iterative-improve good-enouth? improve) 1.0))

(sqrt 49)


  