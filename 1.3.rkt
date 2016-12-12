;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname |1.3|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
(define (sum-of-squares x y)
  ( + (* x x) (* y y)))

(define (myfunction a b c)
   (cond (( and ( < a b) (< a c)) (sum-of-squares b c))
         (( and ( < b a) (< b c)) (sum-of-squares a c))
         (else (sum-of-squares a b))))

(myfunction 2 3 4)
(myfunction 4 5 2)
(myfunction 2 2 2)
(myfunction 4 1 6)