#lang racket

(define (random-in-range low high)
  (let ((range (- high low)))
    ;(min high (max low (+ low (random) (random range))))))
    (+ low (* (random) range))))

(define (predicate a b r)
  (define (square x) (* x x))
  (lambda (x y) (<= (- (+ (square (- x a)) (square (- y b))) (square r)) 0)))

(define (estimate-integral predicate x1 x2 y1 y2 trials)
  (monte-carlo trials
               (lambda () (predicate (random-in-range x1 x2) (random-in-range y1 y2)))))
  
(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond ((= trials-remaining 0)
           (exact->inexact(/ trials-passed trials)))
          ((experiment)
           (iter (- trials-remaining 1) ( + trials-passed 1)))
          (else
           (iter (- trials-remaining 1) trials-passed))))
  (iter trials 0))


(define (pi-by-montecarlo predicate a b r x1 x2 y1 y2 trials)
  (/
   (*
    (estimate-integral (predicate a b r) x1 x2 y1 y2 trials)
    (* (- x2 x1) (- y2 y1)))
   (* r r)))

(estimate-integral (predicate 5 7 3) 2 8 4 10 10000)
(pi-by-montecarlo predicate 5 7 3 2 8 4 10 100000)


