#lang racket

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (enumerate-interval low high)
  (if (> low high)
      null
      (cons low (enumerate-interval (+ low 1) high))))

(define (make-pair-sum pair)
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))


(define (flatmap proc seq)
  (accumulate append null (map proc seq)))


(define (unique-triples n s)
  (map make-pair-sum
       (filter (lambda (pair) (= s (+(car pair) (cadr pair))))
               (flatmap
                (lambda (i)
                  (map (lambda (j) (list i j))
                       (enumerate-interval 0 i)))
                (enumerate-interval 0 n)))))

(unique-triples 5 5)

