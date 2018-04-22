#lang racket

(define (last-pair array)
  (if (null? array)
      null
      (let ((first (car array))
            (last (cdr array)))    
        (if (null? last)
            (list first)
            (last-pair last)))))

(last-pair (list 23 72 149 34))
(last-pair (list 23 ))
(last-pair null)