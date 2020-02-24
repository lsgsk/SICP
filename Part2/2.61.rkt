#lang racket
(define (element-of-set? x set)
  (cond ((null? set) false)
        ((= x (car set)) true)
        ((< x (car set)) false)
        (else (element-of-set? x (cdr set)))))

(define (intersection-set set1 set2)
  (if (or (null? set1) (null? set2))
      '()
      (let ((x1 (car set1))  (x2 (car set2)))
        (cond ((= x1 x2)
               (cons x1
                     (intersection-set (cdr set1)
                                       (cdr set2))))
              ((< x1 x2)
               (intersection-set (cdr set1) set2))
              ((< x2 x1)
               (intersection-set set1 (cdr set2)))))))

(define (adjoin-set x set)
  (let ((head
         (cond ((null? set) '())
               (else (car set)))))
   (cond ((null? set) (cons x '()))
         ((= x head) set)
         ((> x head) (cons head (adjoin-set x (cdr set))))
         ((< x head) (cons x set)))
   ))


;;(element-of-set? 8 (list 1 2 5 8 9 10))
;;(intersection-set '(1 5 8 10 12) '(2 5 9 10))
(adjoin-set 5 '())
(adjoin-set 0 (list 1 2 5))
(adjoin-set 2 (list 1 2 5))
(adjoin-set 3 (list 1 2 5))
(adjoin-set 8 (list 1 2 5))