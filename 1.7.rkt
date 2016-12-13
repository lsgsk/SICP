;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname |1.7|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
(define (average x y);среднее
  (/ (+ x y ) 2))
(define (improve guess x);находим среднее из пирближения и частного (третий столбик)
  (average guess (/ x guess)))

(define (good-enought? guess lastguess);возводим полученное приближение в квадрат и смотрим, насколько они отличаются
  (< (abs(- guess lastguess)) 0.000000000000001))

(define (sqrt-iter-battery guess lastguess x)
  (if (good-enought? guess lastguess)
      guess
   (sqrt-iter-battery (improve guess x) guess x)))

(define (sqrt-iter x)
  (sqrt-iter-battery 1 2 x))

 (sqrt-iter 0.0001)
 (sqrt-iter 16)
 (sqrt-iter 4)