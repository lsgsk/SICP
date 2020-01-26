#lang racket

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      null
      (cons (accumulate   op init (map (lambda (x) (car x)) seqs))
            (accumulate-n op init (map (lambda (x) (cdr x)) seqs)))))

(define matrix `((1 2 3 4) (4 5 6 6) (6 7 8 9)))

(define (dot-product v w)
  (accumulate + 0 (map * v w)))
;(dot-product (list 1 2 3) (list 4 5 6))

(define (column-vector-to-line vector)
  (cond ((null? vector) null)
        ((not(pair? vector)) (list vector))
        (else (append (column-vector-to-line (car vector)) (column-vector-to-line(cdr vector))))))

(define (matrix-*-vector m v)
  (map (lambda(x)(dot-product x v)) m))
;(matrix-*-vector `((2 4 0)(-2 1 3)(-1 0 1)) `(1 2 -1))

(define (transpose mat)
  (accumulate-n cons null mat))
;(transpose matrix)

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
     (map (lambda(x)(matrix-*-vector cols x)) m)))

(matrix-*-matrix `((2 4 0)(-2 1 3)(-1 0 1)) `((1) (2) (-1)))




















