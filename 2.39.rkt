#lang racket

(define (fold-right op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (fold-right op initial (cdr sequence)))))

(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))


(define (revers-right seq)
  (fold-right (lambda (x y)(append y (list x))) null seq))

(define (revers-left seq)
  (fold-left (lambda (x y)(cons y x))null seq))

(revers-right (list 1 2 3 4 5 ))
(revers-left (list 1 2 3 4 5 ))