#lang racket

(define f
  (let ((count 0))
    (lambda (x)
      (if (= count 0)
          (begin
            (set! count (+ count 1))
            x)
          0))))

;(+ (f 0) (f 1)) 
;(+ (f 1) (f 0))