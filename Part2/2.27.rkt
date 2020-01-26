#lang racket
(define (deep-revers array)
  (cond
    ((null? array) null)
    ((not (pair? array)) array)
    (else (append (deep-revers (cdr array)) (list (deep-revers(car array)))))))

        
(deep-revers `(1 2 3))
(deep-revers `((1 2) (3 4)))
(deep-revers `( 0 (1 2) (3 4) 5 (6 (7 8 9) 10 )))
