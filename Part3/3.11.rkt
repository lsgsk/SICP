#lang racket

(define (make-account init-balance)
  (let ((balance init-balance))
    (define (withdraw amount)
      (if (>= balance amount)
          (begin (set! balance (- balance amount))
                 balance)
          "Недостаточно денег на счете"))
    (define (deposit amount)
      (set! balance (+ balance amount))
      balance)
    (define (dispatch m)
      (cond ((eq? m 'withdraw) withdraw)
            ((eq? m 'deposit) deposit)
            (else (error "Неизвестный вызов -- MAKE-ACCOUNT"
                         m))))
    dispatch))

(define acc (make-account 50))
((acc 'deposit) 40)
((acc 'withdraw) 60)