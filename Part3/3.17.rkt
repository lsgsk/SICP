#lang planet neil/sicp

(define (count-pairs x)
  (let ((encountered '()))
    (define (helper x)
      (if (or (not (pair? x)) (memq x encountered))
          0
          (begin
            (set! encountered (cons x encountered))
            (+ (helper (car x))
               (helper (cdr x))
               1))))
    (helper x)))


(define x1 '(1 2 3))
(count-pairs x1)

(define a '(1))
(define b (cons a a))
(define x2 (list b))
(count-pairs x2)

(define q '(1))
(define w (cons q q))
(define x3 (cons w w))
(count-pairs x3)

(define x4 '(1 2 3))
(set-cdr! (cddr x4) x4)
(count-pairs x4)

(display "end")