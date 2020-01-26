#lang racket

(define (make-monitored f)
  (define (make-monitored-count count)
    (define (function-with-count value)
      (begin
        (set! count (+ 1 count))
        (f value)))
    (define (clear)
      (set! count 0))
    (define (dispatch m)
      (cond ((eq? m 'how-many-calls?) count)
            ((eq? m 'reset-count) (clear))
            (else (function-with-count m))))
    dispatch)
  (make-monitored-count 0))


(define sqrtWithCount (make-monitored sqrt))
(sqrtWithCount 225)
(sqrtWithCount 100)
(sqrtWithCount 'how-many-calls?)
(sqrtWithCount 144)
(sqrtWithCount 'how-many-calls?)

(sqrtWithCount 'reset-count)
(sqrtWithCount 'how-many-calls?)