#lang racket

(define (cons x y)
  (lambda (m) (m x y)))

(define (car z)
  (z (lambda (p q) p)))
(define (cdr z)
  (z (lambda (p q) q)))

;ах ты ж ебаный ты наху
(car (cons 5 8))
(cdr (cons 5 8))