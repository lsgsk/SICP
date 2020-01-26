#lang racket
(define (revers array)
  (if (null? array)
      array
      (if (and (not (null? (car array))) (null? (cdr array)))
          (list (car array))
          (append (revers (cdr array)) (list (car array))))))

(list 1 2 3 4 5 6)
(revers (list 1 2 3 4 5 6))
(revers (list 1))
(revers null)
