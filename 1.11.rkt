;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname |1.11|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
(define (fr n)
  (cond ((< n 3) n)
        (else (+
               (fr (- n 1))
               (fr (- n 2))
               (fr (- n 3))))))


(define (fiter fn fn1 fn2 c n)
  (if (= c n)
      fn       
      (fiter (+ fn fn1 fn2) fn fn1 (+ c 1) n)))

(define (fi n)



  
  (if (< n 3)
      n
      (fiter 3 2 1 3 n)))

(fr 10)
(fi 10)