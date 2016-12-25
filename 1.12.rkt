#lang racket
(define (binomial n m)  
  (define (fact n)
    (define (iter product counter)
      (if (> counter n)
          product
          (iter (* counter product)
                (+ counter 1))))
    (iter 1 1))
  
  ( / ( fact n) (* (fact m) (fact (- n m)))))


(define (pascalline x y)
  (if (> x y)
      (newline)
      (begin
        (display (binomial y x))
        (display " ")
        (pascalline (+ 1 x) y)      
        )))



(define (pascal maxline)
  (define (pascaliter line)
    (if (= line maxline)
        (display "finish")
        (begin
          (pascalline 0 line)
          (pascaliter ( + 1 line)))))
  (pascaliter 0))
         
(pascal 10)

      