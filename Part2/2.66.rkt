#lang racket

(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right) (list entry left right))

(define (list->tree elements)
  (define (partial-tree elts n)
    (if (= n 0)
        (cons '() elts)
        (let ((left-size (quotient (- n 1) 2)))
          (let ((left-result (partial-tree elts left-size)))
            (let ((left-tree (car left-result))
                  (non-left-elts (cdr left-result))
                  (right-size (- n ( + left-size 1))))
              (let ((this-entry (car non-left-elts))
                    (right-result (partial-tree (cdr non-left-elts)
                                                right-size)))
                (let ((right-tree (car right-result))
                      (remaining-elts (cdr right-result)))
                  (cons (make-tree this-entry left-tree right-tree)
                        remaining-elts))))))))
  (car (partial-tree elements (length elements))))



(define tree (list->tree '(1 3 5 7 9)))

(define (key x) x)

(define (lookup given-key set-of-records)
  (cond ((null? set-of-records)
         false)
        ((equal? given-key (key (entry set-of-records)))
         (entry set-of-records))
        ((> given-key (key (entry set-of-records)))
         (lookup given-key (right-branch set-of-records)))
        ((< given-key (key (entry set-of-records)))
         (lookup given-key (left-branch set-of-records)))))


(lookup 7 tree)
(lookup 2 tree)

  