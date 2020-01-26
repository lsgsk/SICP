#lang racket

(define (tan-cf x k)  
  (define (tan x d k)
    (let ((inc (- k 1))
          (sq (* x x)))
      (if( = k 0)
         (/ sq d)
         (/ sq (- d (tan x (+ d 2) inc))))))
    (/ x (- 1 (tan x 3 (- k 1)))))


(tan-cf 1.0 100)