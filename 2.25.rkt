#lang racket
(car (cdr (car (cdr ( cdr (list 1 3 (list 5 7) 9))))))
(car ( car ( list (list 7))))
(car(cdr(car(cdr(car(cdr(car(cdr(car(cdr(car(cdr`(1(2(3(4(5(6 7))))))))))))))))))