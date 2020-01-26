#lang racket

(define (factorial-r n)
  (if (= n 1)
      1
      (* n (factorial-r (- n 1)))))

(define (factorial-i n)
  (define (fact-iter product counter max-count)
    (if (> counter max-count)
        product
        (fact-iter (* counter product)
                   (+ counter 1) max-count)))
  (fact-iter 1 1 n))


(factorial-r 6)
(factorial-i 6)