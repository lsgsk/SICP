#lang racket

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x(cdr set)))))

(element-of-set? 3 (list 2 3 2 1 3 2 2))
(element-of-set? 5 (list 2 3 2 1 3 2 2))

(define (adjoin-set x set)(cons x set))
(adjoin-set 3 (list 2 3 2 1 3 2 2))
(adjoin-set 5 (list 2 3 2 1 3 2 2))

(define(intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
        ((element-of-set? (car set1) set2)
         (cons (car set1)
               (intersection-set (cdr set1) set2)))
        (else (intersection-set (cdr set1) set2))))



(define (union-set set1 set2)
  (append set1 set2))
     
(union-set (list 1 2 3) (list 2 3 4))

