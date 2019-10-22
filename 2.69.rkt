#lang racket

(define(make-leaf symbol weight)
  (list 'leaf symbol weight))

(define (leaf? object)
  (eq? (car object) 'leaf))

(define (symbol-leaf x) (cadr x))

(define (weight-leaf x) (caddr x))

(define (make-code-tree left right)
  (list left right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))

(define (left-branch tree) (car tree))

(define (right-branch tree) (cadr tree))

(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))

(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))

(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
        '()
        (let ((next-branch
               (choose-branch (car bits) current-branch)))
          (if (leaf? next-branch)
              (cons (symbol-leaf next-branch)
                    (decode-1 (cdr bits) tree))
              (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))

(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch branch))
        ((= bit 1) (right-branch branch))
        (else (error "Плохой бит -- CHOOSE-BRANCH" bit))))

(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) (cons x set))
        (else (cons (car set)
                    (adjoin-set x (cdr set))))))

(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (adjoin-set (make-leaf (car pair)
                               (cadr pair))
                    (make-leaf-set (cdr pairs))))))


(define sample-tree
  (make-code-tree (make-leaf 'A 4)
                  (make-code-tree (make-leaf 'B 2)
                                  (make-code-tree (make-leaf 'D 1)
                                                  (make-leaf 'C 1)))))
(define sample-message '(A D A B B C A))

;   abcd-8
;  /     \
;a-4    bcd-4
;       /   \
;     b-2    cd -2
;            /   \
;          c-1    d-1

               
(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x(cdr set)))))


(define (encode-symbol symb tree)
  (cond ((leaf? tree) '())
        ((element-of-set? symb (symbols (left-branch tree))) (cons 0 (encode-symbol symb (left-branch tree))))
        ((element-of-set? symb (symbols (right-branch tree))) (cons 1 (encode-symbol symb (right-branch tree))))
        (else "Ошибка. Неизвестный символ")))

(define (generate-huffman-tree pairs)
  (define (successive-merge leaf-set)
    (define (extract-minimal-leaf set)
      (define (inner-extract-minimal set accumulate min)
        (if (null? set)
            (cons min accumulate)
            (let ((min-weight (if (leaf? min) (weight-leaf min) (weight min))))
              (let ((car-weight (if (leaf? (car set)) (weight-leaf (car set)) (weight (car set)))))
                (cond
                  ((< min-weight car-weight) (inner-extract-minimal (cdr set) (cons (car set) accumulate) min))
                  (else (inner-extract-minimal (cdr set) (cons min accumulate) (car set))))))))
      (cond ((null? set) '())
            (else (inner-extract-minimal (cdr set) '() (car set)))))
  
    (cond ((null? leaf-set) leaf-set)
          ((null?(cdr leaf-set)) (car leaf-set))
          (else 
           (let ((minimal-on-top (extract-minimal-leaf leaf-set)))
             (let ((first (car minimal-on-top)))
               (let ((minimal-on-top (extract-minimal-leaf (cdr minimal-on-top))))
                 (let ((second (car minimal-on-top)))
                   (let ((tree (make-code-tree first second)))
                     (let ((combine (append (list tree) (cddr leaf-set))))
                       (successive-merge combine))))))))))
  (successive-merge (make-leaf-set pairs)))

(generate-huffman-tree '())
(generate-huffman-tree '((C 1)))
(generate-huffman-tree '((A 4) (B 2) (D 1) (C 1)))
