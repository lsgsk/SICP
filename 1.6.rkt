;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname |1.6|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
(define (square x); возведение в квадрат
    (* x x))
(define (average x y);среднее
  (/ (+ x y ) 2)) 
(define (good-enought? guess x);возводим полученное приближение в квадрат и смотрим, насколько они отличаются
  (< (abs(- (square guess) x)) 0.000000000000001))
(define (improve guess x);находим среднее из пирближения и частного (третий столбик)
  (average guess (/ x guess)))
(define (sqrt-iter guess x)
  (if (good-enought? guess x)
      guess
   (sqrt-iter (improve guess x) x)))

 (sqrt-iter 1 0.0001)
 (sqrt-iter 1 16)
 (sqrt-iter 1 4)

(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause)
        (else else-clause)))

(define (sqrt-iter-with-new-if guess x)
  (new-if (good-enought? guess x) guess (sqrt-iter-with-new-if (improve guess x) x)))
;это фактически обычная функция. при аппликативном порядке перед вызовом вычисляются все аргументы
;т.е. мы рекурсивно вызываем себя при попытке выполнения ифа 
;в игоге уходим очень глубоко в бесконечность

;(new-if (= 2 3) 0 5)
;(sqrt-iter-with-new-if 1 2)