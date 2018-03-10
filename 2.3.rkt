#lang racket
;Точки
(define (make-point x y)(cons x y))
(define (x-point p)(car p))
(define (y-point p)(cdr p))
(define (print-point p)
  (newline)
  (display "(")
  (display(x-point p))
  (display ",")
  (display(y-point p))
  (display ")"))
;Прямоугольники
(define (make-rectangle point width height)(cons point (cons width height)))
(define (rectangle-topleft rect)(car rect))
(define (rectangle-width rect)(car (cdr rect)))
(define (rectangle-height rect)(cdr (cdr rect)))

;(define (make-rectangle topleft bottomright)(cons topleft bottomright))
;(define (rectangle-topleft rectangle)(car rectangle))
;(define (rectangle-bottomright rectangle)(cdr rectangle))
;(define (rectangle-width rectangle) 
;  (abs (- (x-point (rectangle-topleft rectangle)) 
;          (x-point (rectangle-bottomright rectangle)))))
;(define (rectangle-height rectangle) 
;  (abs (- (y-point (rectangle-topleft rectangle)) 
;          (y-point (rectangle-bottomright rectangle)))))

(define (rectangle-perimetr rect) (* 2 (+ (rectangle-width rect) (rectangle-height rect))))
(define (rectangle-area rect) (* (rectangle-width rect) (rectangle-height rect)))


(define rectangle (make-rectangle (make-point 5 10) 3 4))
(rectangle-perimetr rectangle)
(rectangle-area rectangle)
