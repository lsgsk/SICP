#lang racket

(define (put x y z)
  (display "#"))
(define (get x y z)
  (display "%"))

(define (variable? x) (symbol? x))
(define (same-variable? v1 v2) (and (variable? v1) (variable? v2) (eq? v1 v2)))


(define (operator exp) (car exp))
(define (operands exp) (cdr exp))

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0)) (else ((get 'deriv (operator exp)) (operands exp)
                                                                                               var))))

(define (install-package) 
  (define (make-sum a1 a2) 
    (list '+ a1 a2)) 
  (define (addend s) (car s)) 
  (define (augend s) (cadr s)) 
  (define (deriv-sum exp var) 
    (make-sum (deriv (addend exp) var) 
              (deriv (augend exp) var))) 
  (define (make-product m1 m2) 
    (list '* m1 m2)) 
  (define (multiplier p) (car p)) 
  (define (multiplicand p) (cadr p)) 
  (define (deriv-product exp var) 
    (make-sum 
     (make-product (multiplier exp) 
                   (deriv (multiplicand exp) var)) 
     (make-product (deriv (multiplier exp) var) 
                   (multiplicand exp)))) 
  (put 'deriv '+ deriv-sum) 
  (put 'deriv '* deriv-product) 
  'done)
