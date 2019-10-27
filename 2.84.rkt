#lang planet neil/sicp
;таблица хранения типов, украдена из следующих глав
(define (make-table)
  (let ((local-table (list '*table*)))
    (define (lookup key-1 key-2)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record (assoc key-2 (cdr subtable))))
              (if record
                  (cdr record)
                  false))
            false)))
    (define (insert! key-1 key-2 value)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record (assoc key-2 (cdr subtable))))
              (if record
                  (set-cdr! record value) (set-cdr! subtable
                                                    (cons (cons key-2 value) (cdr subtable)))))
            (set-cdr! local-table (cons (list key-1
                                              (cons key-2 value)) (cdr local-table)))))
      'ok)
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Неизвестная операция -- TABLE" m))))
    dispatch))

(define operation-table (make-table))
(define get (operation-table 'lookup-proc))
(define put (operation-table 'insert-proc!))

(define get-coercion (operation-table 'lookup-proc))
(define put-coercion (operation-table 'insert-proc!))

(define (attach-tag type-tag contents)
  (cons type-tag contents))
       
(define (type-tag datum)
  (cond ((pair? datum) (car datum))
        (else (error "Некорректные помеченные данные -- TYPE-TAG" datum))))

(define (contents datum)
  (cond ((pair? datum) (cdr datum))
        (else (error "Некорректные помеченные данные -- CONTENTS" datum))))

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))     
          (if (= (length args) 2)
              (let ((arg1 (car args))(arg2 (cadr args)))
                (cond ((canBeUpped arg1 arg2) (apply-generic op (rise arg1) arg2))
                      ((canBeUpped arg2 arg1) (apply-generic op arg1 (rise arg2)))
                      (else (error "Нет метода для этих типов" (list op type-tags)))))
              (error "Нет метода для этих типов" (list op type-tags)))))))

(define (canBeUpped type1 type2)
  (define (apply-generic op . args)
    (let ((type-tags (map type-tag args)))
      (let ((proc (get op type-tags)))
        (if proc
            (apply proc (map contents args))
            false))))
  (if (eq? (type-tag type1) (type-tag type2))
      true
      (let ((wideType (apply-generic 'rise type1)))
        (if wideType
            (canBeUpped wideType type2)
            false))))

;обобщенные процедуры
(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))
(define (rise x) (apply-generic 'rise x))

;пакет натуральной арифметики
(define (install-natural-package)
  (define (tag x)
    (attach-tag 'natural x))
  (define (natural->rational x)
    (make-rational-number x 1))
  (put 'add '(natural natural)
       (lambda (x y) (tag (+ x y))))
  (put 'sub '(natural natural)
       (lambda (x y) (tag (- x y))))
  (put 'mul '(natural natural)
       (lambda (x y) (tag (* x y))))
  (put 'div '(natural natural)
       (lambda (x y) (tag (/ x y))))
  (put 'rise '(natural)
       (lambda (x) (natural->rational x)))
  (put 'make 'natural
       (lambda (x) (tag x)))
  "natural installed")

;пакет рациональных чисел
(define (install-rational-package)
  ;; внутренние процедуры
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  (define (make-rat n d)
    (let ((g (gcd n d)))
      (cons (/ n g) (/ d g))))
  (define (add-rat x y)
    (make-rat (+ (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (sub-rat x y)
    (make-rat (- (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (mul-rat x y)
    (make-rat (* (numer x) (numer y))
              (* (denom x) (denom y))))
  (define (div-rat x y)
    (make-rat (* (numer x) (denom y))
              (* (denom x) (numer y))))
  (define (rational->real x)
    (make-real-number ( exact->inexact(/ (numer x) (denom x)))))
  ;; интерфейс к остальной системе
  (define (tag x) (attach-tag 'rational x))
  (put 'add '(rational rational)
       (lambda (x y) (tag (add-rat x y))))
  (put 'sub '(rational rational)
       (lambda (x y) (tag (sub-rat x y))))
  (put 'mul '(rational rational)
       (lambda (x y) (tag (mul-rat x y))))
  (put 'div '(rational rational)
       (lambda (x y) (tag (div-rat x y))))
  (put 'rise '(rational)
       (lambda (x) (rational->real x)))
  (put 'make 'rational
       (lambda (n d) (tag (make-rat n d))))
  "rational installed")

;пакет вещественной арифметики
(define (install-real-package)
  (define (tag x)
    (attach-tag 'real x))
  (put 'add '(real real)
       (lambda (x y) (tag (+ x y))))
  (put 'sub '(real real)
       (lambda (x y) (tag (- x y))))
  (put 'mul '(real real)
       (lambda (x y) (tag (* x y))))
  (put 'div '(real real)
       (lambda (x y) (tag (/ x y))))
  (put 'make 'real
       (lambda (x) (tag x)))
  "real installed")

;=========================================
(define (make-natural-number n) ((get 'make 'natural) (inexact->exact (floor n))))
(define (make-rational-number n d) ((get 'make 'rational) n d))
(define (make-real-number n) ((get 'make 'real) n))
;=========================================

(install-natural-package)
(install-rational-package)
(install-real-package)

(define a (make-natural-number 3))
(define b (make-rational-number 4 3))
(define c (make-real-number 3.8))

(canBeUpped a b)
(canBeUpped a c)
(canBeUpped b c)
(canBeUpped c a)

(add a a)
(add a b)
(add c b)
(sub c c)