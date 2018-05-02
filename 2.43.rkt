#lang racket

(define (accumulate op initial sequence)  (if (null? sequence)      initial      (op (car sequence)          (accumulate op initial (cdr sequence)))))
(define (flatmap proc seq)  (accumulate append null (map proc seq)))
(define (enumerate-interval low high)  (if (> low high)      null      (cons low (enumerate-interval (+ low 1) high))))


(define empty-board (list))

(define (safe? new-position positions)
  (define (cols-safe? value array)
    (cond ((null? array) true)
          ((not(pair? array)) ( not (= array value) ))
          (else (and (cols-safe? value (car array)) (cols-safe? value (cdr array))))))

  (define (diag-safe? x y array)
    (cond ((null? array) true)
          (( = (+ x y) (+ (length array) (car array))) false)
          (( = (- x y) (- (length array) (car array))) false)
          (else (and (diag-safe? x y (cdr array)) (diag-safe? x y (cdr array))))))

  (and (cols-safe? (car positions) (cdr positions))
       (diag-safe? new-position (car positions) (cdr positions))))

(define (adjoin-position new-row k rest-of-queens)
  (begin
    (display (list k rest-of-queens))
    (newline)
    (cons new-row rest-of-queens)))

(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (positions) (safe? k positions))
         (flatmap
          (lambda (new-row)
            (map (lambda (rest-of-queens)
                   (adjoin-position new-row k rest-of-queens))
                 (queen-cols (- k 1))))                 
          (enumerate-interval 1 board-size)))))    
  (queen-cols board-size))

(queens 4)





