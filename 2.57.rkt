#lang racket
(define (variable? x) (symbol? x))
(define (same-variable? v1 v2) (and (variable? v1) (variable? v2) (eq? v1 v2)))
(define (=number? exp num) (and (number? exp) (= exp num)))
(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list '+ a1 a2))))
(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list '* m1 m2))))        
(define (sum? x) (and (pair? x) (eq? (car x) '+)))

(define (addend s) (cadr s))
(define (augend s)
  (cond ((null? (cddr s)) 0)
        ((pair? (cddr s)) (make-sum (car (cddr s)) (augend (cons '+ (cddr s)))))))

;(augend '(+ 5 2 3))
;(augend '(+ 5 x 3))
;(augend '(+ 5 2 3 4 1))
;(augend '(+ 5 2 x y 2))
;(augend '(+ 5 2 x y 2 ( + 3 4 z)))                           


(define (product? x) (and (pair? x) (eq? (car x) '*)))
(define (multiplier p) (cadr p))
(define (multiplicand p)
  (cond ((null? (cddr p)) 1)
        ((pair? (cddr p)) (make-product (car (cddr p)) (multiplicand (cons '+ (cddr p)))))))

;(multiplicand '(* 5 2 3))
;(multiplicand '(* 5 x 3))
;(multiplicand '(* 5 2 3 4 1))
;(multiplicand '(* 5 2 x y 2))
;(multiplicand '(* 5 2 x y 2 (* 3 4 z)))  


(define (exponentiation? x) (and (pair? x) (eq? (car x) '^)))
(define (base x) (cadr x))
(define (exponent x) (caddr x))
(define (make-exponentiation x n)
  (cond ((=number? n 0) 1)
        ((=number? n 1) x)
        (else (list '^ x n))))

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
          (make-product (multiplier exp)
                        (deriv (multiplicand exp) var))
          (make-product (deriv (multiplier exp) var)
                        (multiplicand exp))))
        ((exponentiation? exp)
         (make-product (exponent exp) (make-product (make-exponentiation (base exp) (- (exponent exp) 1)) (deriv (base exp) var))))
        (else
         (error "неизвестный тип выражения -- DERIV" exp))))

(deriv '(* x y (+ x 3)) 'x)




