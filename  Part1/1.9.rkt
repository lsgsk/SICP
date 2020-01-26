;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname |1.9|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
(define (inc x)(+ 1 x))
(define (dec x)(- x 1)) 

(define (sum1 a b)
  (if (= a 0)
       b
       (inc (sum1 (dec a) b))))

(define (sum2 a b)
  (if ( = a 0)
      b
      (sum2 (dec a) (inc b))))


(sum1 1 10)
(sum2 4 5)
