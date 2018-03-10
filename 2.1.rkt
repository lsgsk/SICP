#lang racket
(define (make-rat n d)
  (define (multiplication-sign x y)(if (>=(* x y) 0) 1 -1))
  (define (convert-to-positive-number x)         (if (>= x 0) x (* -1 x)))
  (let ((normal-n (* (convert-to-positive-number n) (multiplication-sign n d)))           
        (normal-d (convert-to-positive-number d))
        (g (gcd (convert-to-positive-number n) (convert-to-positive-number d))))
    (cons (/ normal-n g) (/ normal-d g))))

(define (numer x) (car x))
(define (demon x) (cdr x))

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (demon x)))


(define (add-rat x y)
  (make-rat (+ (* (numer x) (demon y))(* (numer y) (demon x)))
            (* (demon x) (demon y))))

(define (sub-rat x y)
  (make-rat (- (* (numer x) (demon y))(* (numer y) (demon x)))
            (* (demon x) (demon y))))

(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
            (* (demon x) (demon y))))

(define (div-rat x y)
  (make-rat (* (numer x) (demon y))
            (* (demon x) (numer y))))

(define one-half (make-rat 1 2))
(define one-third (make-rat 1 -3))

(print-rat (make-rat -1 2))
(print-rat (make-rat 1 -2))
(print-rat (make-rat -1 -2))
(print-rat (add-rat one-half one-third))
(print-rat (mul-rat one-half one-third))
(print-rat (add-rat one-third one-third))

