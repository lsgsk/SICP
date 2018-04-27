#lang racket


(define (square-tree tree)
  (define (square x) (* x x))
  (define (tree-map pred tree)
    (cond ((null? tree) null)
          ((not (pair? tree)) (pred tree))
          (else (cons (tree-map pred (car tree)) (tree-map pred (cdr tree))))))
  (tree-map square tree))

(square-tree (list 1 (list 2 (list 3 4) 5) (list 6 7)))