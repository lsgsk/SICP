#lang racket
;a
(define (make-mobile left right)
  (list left right))

(define (make-branch length structure)
  (list length structure))

(define (left-branch mobile)
  (car mobile))

(define (right-branch mobile)
  (car (cdr mobile)))

(define (branch-length branch)
  (car branch))

(define (branch-structure branch)
  (car(cdr branch)))

;b
(define (is-branch mobile)
  (if (and (pair? mobile)
           (not (pair? (car mobile)))
           (not (pair? (car (cdr mobile)))))
      true
      false))

(define (branch-weight branch) 
  (let ((struct (branch-structure branch))) 
    (if (pair? struct) 
        (total-weight struct) 
        struct)))

(define (total-weight mobile)
  (cond ((null? mobile) null)
        ((is-branch mobile) (branch-length mobile))
        (else (+ (total-weight (left-branch mobile))(total-weight(right-branch mobile))))))

;c
(define (balanced? mobile) 
  (define (branch-balanced? branch) 
    (if (pair? (branch-structure branch)) 
        (balanced? (branch-structure branch)) 
        true)) 
  (define (torque branch) 
    (* (branch-length branch) (branch-weight branch))) 
  (let ((left (left-branch mobile)) 
        (right (right-branch mobile))) 
    (and (branch-balanced? left) 
         (branch-balanced? right) 
         (= (torque left) (torque right)))))
