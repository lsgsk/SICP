#lang racket
(require rnrs/base-6)
(require rnrs/mutable-pairs-6)

(define (make-cycle x)
  (define (last-pair x)
    (if (null? (cdr x))
        x
        (last-pair (cdr x))))
  (set-cdr! (last-pair x) x)
  x)

(define (check-cycle x)
  (define (speed1 lst)
    (cdr lst))
  (define (speed2 lst)
    (if (not (null? (cdr lst)))
        (cddr lst)
        null))
  (define (race lst1 lst2)
    (let ((x1 (speed1 lst1))
          (x2 (speed2 lst2)))
      (begin
        (display "1->")
        (display x1)
        (newline)
        (display "2->")
        (display x2)
        (newline)
        (cond ((or (null? x1) (null? x2)) false)
              ((eq? x1 x2) true)
              (else (race x1 x2))))))
  (race x x))
    
(define list1 (list 'a 'b 'c 'd))
(define list2 (make-cycle (list 'a 'b 'c 'd)))

(check-cycle list1)
(check-cycle list2)