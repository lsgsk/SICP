#lang racket
(define (pow x n)
  (define (power x n res)(if( = n 0) res (power x (- n 1) (* res x))))
  (power x n 1))

(define (cons x y)
  (* (pow 2 x) (pow 3 y)))

(define (remove_divider x divider)
  (if (= (remainder x divider) 0)
      (remove_divider (/ x divider) divider)
      x))

(define (pow-of-number x number)
  (define (pon x count)
    (if (= x 1)
        count
        (pon (/ x number) (+ 1 count))))
  (pon x 0))

(define (car z)  
  (pow-of-number (remove_divider z 3) 2))

(define (cdr z)  
  (pow-of-number (remove_divider z 2) 3))

(car (cons 0 0)) 
(car (cons 3 4)) 
(car (cons 5 5)) 
(car (cons 1 4)) 
(car (cons 4 8)) 

(display "/////////////")
(newline)
(cdr (cons 0 0))
(cdr (cons 3 4))
(cdr (cons 5 5))
(cdr (cons 1 4))
(cdr (cons 4 8))