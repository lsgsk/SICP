;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname |1.8|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
(define (improve y x)
  (/ ( + (/ x(* y y )) (* 2 y)) 3))

(define (good-enought? guess lastguess)
  (< (abs(- guess lastguess)) 0.0000001))

(define (sqrt3-iter-battery guess lastguess x)
  (if (good-enought? guess lastguess)
      guess
   (sqrt3-iter-battery (improve guess x) guess x)))

(define (sqrt3-iter x)
  (sqrt3-iter-battery 1 2 x))

 (sqrt3-iter 0.001)
 (sqrt3-iter 8)
 (sqrt3-iter 27)