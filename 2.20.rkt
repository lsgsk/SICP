#lang racket
(define (same-parity a . b) 
  (define (inner-same-parity parity array)
    (if (null? array)
        null        
        (append
         (if( = (remainder (car array) 2) parity)
            (list (car array))
            null)
         (inner-same-parity parity (cdr array)))))           

  (if (null? a)
      null
      (append (list a) (inner-same-parity ( if (=(remainder a 2) 0)  0  1)  b))))


(same-parity 3 4 5 6 7 8)
(same-parity 2 3 4 5 6 7 8)
(same-parity 2)
(same-parity 2 3)
(same-parity null)
       
