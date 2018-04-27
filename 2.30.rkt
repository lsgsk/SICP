#lang racket

(define (square-tree tree)
  (define (inner-square-tree tree pred)
    (cond ((null? tree) null)
          ((not (pair? tree)) (pred tree))
          (else (cons (inner-square-tree (car tree) pred) (inner-square-tree (cdr tree) pred)))))
  (inner-square-tree tree (lambda (x) (* x x))))

(define (square-tree-2 tree)
  (map (lambda (sub-tree)
         (cond ((null? tree) null)
               ((not(pair? sub-tree)) (* sub-tree  sub-tree))
               (else (square-tree-2 sub-tree)))) tree))


(square-tree   (list 1 (list 2 (list 3 4) 5) (list 6 7)))
(square-tree-2 (list 1 (list 2 (list 3 4) 5) (list 6 7)))