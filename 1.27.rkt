#lang racket
(define (square x)(* x x))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m)) m))
        (else
         (remainder (* base (expmod base (- exp 1) m)) m))))

(define (ferma-test n m)(= (expmod m n n) m))

(define (lets-fail-ferma-test-for-karmaikl n)
  (define (loop-ferma-test n count)
    (cond (( = count n) true)
          ((ferma-test n count) (loop-ferma-test n (+ 1 count)))
          (else false)))
  (loop-ferma-test n 2))

(lets-fail-ferma-test-for-karmaikl 561)
(lets-fail-ferma-test-for-karmaikl 1105)
(lets-fail-ferma-test-for-karmaikl 1729)
(lets-fail-ferma-test-for-karmaikl 2465)
(lets-fail-ferma-test-for-karmaikl 2821)
(lets-fail-ferma-test-for-karmaikl 6601)
(lets-fail-ferma-test-for-karmaikl 8911)

;{display "---------------"}
;(search-for-primes 10000000000000 10000000000150)
;{display "---------------"}
;(search-for-primes 100000000000000 100000000000100)
;{display "---------------"}
;(search-for-primes 1000000000000000 1000000000000200)