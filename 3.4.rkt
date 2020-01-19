#lang racket

(define (make-account balance init-password)
  (let ((count 0)
        (limits 3))
    (define (withdraw amount)
      (if (>= balance amount)
          (begin (set! balance (- balance amount)) balance)
          "Недостаточно денег на счете"))
    (define (deposit amount)
      (set! balance (+ balance amount))
      balance)
    (define (incorrect-password . args)
      (begin
        (set! count (+ 1 count))
        "Неверный пароль"))
    (define (call-the-cops . args)
      (begin
        (set! count 0)
        "Call the cops"))
    (define (dispatch password m)
      (cond ((eq? count limits) call-the-cops)
            ((and (> limits count)(not (eq? password init-password))) incorrect-password)
            (else
             (cond ((eq? m 'withdraw) withdraw)
                   ((eq? m 'deposit) deposit)
                   (else (error "Неизвестный вызов -- MAKE-ACCOUNT" m))))))
    dispatch))


(define acc (make-account 100 "password"))
((acc "password" 'withdraw) 25)
((acc "password" 'withdraw) 125)
((acc "incorrect" 'withdraw) 25)
((acc "incorrect" 'withdraw) 25)
((acc "incorrect" 'withdraw) 25)
((acc "incorrect" 'withdraw) 25)
((acc "password" 'withdraw) 25)
((acc "password" 'deposit) 10)