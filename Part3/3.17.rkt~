#lang racket
(require rnrs/base-6)
(require rnrs/mutable-pairs-6)

;по-моему я сделать что-то сложное и не то 

(define (make-cycle x)
  (define (last-pair x)
    (if (null? (cdr x))
        x
        (last-pair (cdr x))))
  (set-cdr! (last-pair x) x)
  x)

(define count-pairs
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
    (define (count x)
      (if (not (pair? x))
          0
          (if (contains? listened x)
              0
              (begin
                (add-to-end listened x)  
                (+ (count (car x))
                   (count (cdr x))
                   1))))
      )
    count))
    


(define x1 (cons (cons 1 2) (cons 3 4)))
(define x2 (make-cycle (list 'a 'b 'c)))

(count-pairs x1)
(count-pairs x2)