#lang racket
(define (equal? a b)
  (cond ((and (symbol? a)(symbol? b)) (eq? a b))
        ((and (number? a)(number? b)) (= a b))
        ((and (pair? a)(pair? b)) (and (equal? (car a) (car b)) (equal? (cdr a) (cdr b))))
        ((and (null? a) (null? b) true))
        (else false)))


(equal? '(this is a list) '(this is a list))