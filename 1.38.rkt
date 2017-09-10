#lang racket

(define (cont-frac-leo n)  
  (define (inner-cont-frac-leo i mult n)
    (define (isSpecial x) (= (remainder i 3) 2))
    (let ((nextmult (if (isSpecial i) (+ 2 mult) mult))
          (d (if (= (remainder i 3) 2) mult 1)))
      (if (= n 0)
          (/ 1 d)
          (/ 1 (+ d (inner-cont-frac-leo (+ 1 i) nextmult (- n 1)))))))       
  (inner-cont-frac-leo 1 2.0 n))

(+ 2 (cont-frac-leo 100))
