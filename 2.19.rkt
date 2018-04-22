#lang racket
(define us-coins (list 50 25 10 5 1))
(define uk-coins (list 100 50 20 10 5 2 1 0.5))

(define (no-more? array)(null? array))
(define (except-first-denomination array) (cdr array))
(define (first-denomination array) (car array))
(define (revers array)
  (if (null? array)
      array
      (if (and (not (null? (car array))) (null? (cdr array)))
          (list (car array))
          (append (revers (cdr array)) (list (car array))))))

(define (cc amount coin-values)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (no-more? coin-values)) 0)
        (else
         (+ (cc amount (except-first-denomination coin-values))
            (cc (- amount (first-denomination coin-values)) coin-values)))))

(cc 100 us-coins)
(cc 100 (revers us-coins))
(cc 100 uk-coins)

