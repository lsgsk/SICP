#lang racket

(define (make-account balance init-password)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount)) balance)
        "Недостаточно денег на счете"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (incorrect-password . args)
    "Неверный пароль")
  (define (dispatch password m)
    (if (eq? password init-password)
        (cond ((eq? m 'withdraw) withdraw)
              ((eq? m 'deposit) deposit)
              (else (error "Неизвестный вызов -- MAKE-ACCOUNT" m)))
        incorrect-password))
  dispatch)


(define acc (make-account 100 "password"))

((acc "password" 'withdraw) 25)
((acc "password" 'withdraw) 125)
((acc "incorrect" 'withdraw) 25)
