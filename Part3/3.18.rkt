#lang racket
(require rnrs/base-6)
(require rnrs/mutable-pairs-6)

(define (make-cycle x)
  (define (last-pair x)
    (if (null? (cdr x))
        x
        (last-pair (cdr x))))
  (set-cdr! (last-pair x) x)
  x)

(define check-cycle
  (let ((listened (cons null null)))
    (define (add-to-end list value)
      (if (pair? (cdr list))
          (add-to-end (cdr list) value)
          (set-cdr! list (cons value '()))))
    (define (contains? list value)
      (if (null? list)
          false
          (if (eq? value (car list))
              true
              (contains? (cdr list) value))))
    (define (cycle x)
      (if (not (pair? x))
          false
          (if (contains? listened x)
              true
              (begin
                (add-to-end listened x)  
                (or (cycle (car x))
                    (cycle (cdr x)))))))
    cycle))
    

(define list1 (cons (cons 1 2) (cons 3 4)))
(define list2 (make-cycle (list 'a 'b 'c)))

(check-cycle list1)
(check-cycle list2)