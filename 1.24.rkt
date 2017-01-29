#lang racket
(define (square x)(* x x))
(define (smallest-divisor n)(find-divisor n 2))
(define (divides? a b )(= (remainder b a ) 0))
(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n ) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))

(define (prime? n)(=(smallest-divisor n) n))

(define (timed-prime-test n)
    (begin
(newline)
        (display n)
        (fast-prime?-with-time n (current-milliseconds))
     (start-prime-test n (current-milliseconds))))

(define (start-prime-test n start-time)
  (if (prime? n)
      (begin        
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

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m)) m))
        (else
         (remainder (* base (expmod base (- exp 1) m)) m))))

(define (ferma-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((ferma-test n) (fast-prime? n (- times 1)))
        (else false)))

(define (fast-prime?-with-time n start-time)
  (begin
   (display (ferma-test n)      )
   (display " ")
   (display (- (current-milliseconds) start-time))))

{display "---------------"}
(search-for-primes 1000000000 1000000150)

;(fast-prime? 17 3)
;{display "---------------"}
;(search-for-primes 10000000000000 10000000000150)
;{display "---------------"}
;(search-for-primes 100000000000000 100000000000100)
;{display "---------------"}
;(search-for-primes 1000000000000000 1000000000000200)