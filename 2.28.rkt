#lang racket

(define (fringe array)
  (cond
    ((null? array) null)
    ((not (pair? array)) (list array))
    (else (append (fringe (car array)) (fringe (cdr array))))))
  

(define x (list  (list 1 2) (list 3 4)))
(fringe x)
(fringe (list x x))



