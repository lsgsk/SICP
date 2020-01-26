#lang racket

(define (make-accumulator init-value)
  (lambda (value)
    (begin
      (set! init-value (+ init-value value))
      init-value)))

(define A (make-accumulator 5))
(A 10)
(A 15)