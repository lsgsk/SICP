#lang racket

(define (make-withdraw init-balance)
  (let ((balance init-balance))
    (lambda (amount)
      (if (>= balance amount)
          (begin (set! balance (- balance amount))
                 balance)
          "Недостаточно денег на счете"))))

(define W1 (make-withdraw 100))
(W1 20)
(W1 20)