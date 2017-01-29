#lang racket
(define (square x) (* x x))

(define (next divisor)
   (cond ((= divisor 2) 3)
         (else (+ 2 divisor))))

(define (smallest-divisor n)(find-divisor n 2))
(define (divides? a b )(= (remainder b a ) 0))
(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n ) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (next test-divisor)))))

(define (prime? n)(=(smallest-divisor n) n))

(define (timed-prime-test n)
    (begin

     (start-prime-test n (current-milliseconds))))

(define (start-prime-test n start-time)
  (if (prime? n)
      (begin
         (newline)
        (display n)
        (report-prime (- (current-milliseconds) start-time)))
      (report-not-prime)))


(define (report-prime elapsed-time)(begin(display " *** ")(display elapsed-time)))
(define (report-not-prime)(display ""))

(define (search-for-primes start end)
  (if (>= end start)
      (begin
        (timed-prime-test start)
        (search-for-primes (+ 1 start) end))
      (begin
        (newline)
      (display "end"))))

{display "---------------"}
(search-for-primes 10000000000000 10000000000150)
{display "---------------"}
(search-for-primes 100000000000000 100000000000100)
{display "---------------"}
(search-for-primes 1000000000000000 1000000000000200)