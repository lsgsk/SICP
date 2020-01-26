#lang racket

(define (cont-frac-rec n d k)
  (define (inner-cont-frac n d k i)
    (if (= k 0)
        (/ (n i) (d i))
        (/ (n i) (+ (d i) (inner-cont-frac n d (- k 1) (+ 1 i))))))
  (inner-cont-frac n d k 0))

(define (cont-frac-iter n d k)
  (let ((start (/(n k)(d k))))
    (define (inner-cont-frac-iter n d k r)
      (if (= k 0)
          r        
          (inner-cont-frac-iter n d (- k 1)(/ (n k) (+ (d k) r)))))
    (inner-cont-frac-iter n d k start)))


( / 1 (cont-frac-rec  (lambda (i) 1.0) (lambda (i) 1.0) 11))
( / 1 (cont-frac-iter (lambda (i) 1.0) (lambda (i) 1.0) 11)) 