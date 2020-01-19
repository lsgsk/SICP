#lang racket

(define rand
  (let ((random-init 0))
    (let ((x random-init))
      (lambda (command)
        (define (rand-update x)
          (+ (* 1.23 x) 0.123))
        (cond
          ((eq? command 'generate)
           (begin
             (set! x (rand-update x)) x))
          ((eq? command 'reset)
           (set! x random-init)))))))

(rand 'generate)
(rand 'generate)
(rand 'reset)
(rand 'generate)
(rand 'generate)





