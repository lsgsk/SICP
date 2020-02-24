#lang planet neil/sicp

(define (make-deque)
  (let ((front-ptr '())
        (rear-ptr '()))

    (define (make-node value next prev) (cons value (cons next prev)))
    (define (value node) (car node))
    (define (next node) (cadr node))
    (define (prev node) (caddr node))

    (define (empty-queue?)
      (null? front-ptr))

    (define (print-deque)
      (define (print head)
        (cond ((pair? head)
               (display (value head))
               (display " ")
               (print (next head)))
              (else
               (display "\n"))))
     (print front-ptr))

    (define (front-insert-deque! item)
      (let ((new-node (make-node item '() '())))
        (cond ((empty-queue?)
               (set! front-ptr new-node)
               (set! rear-ptr new-node))
              (else
               (set-cdr! (cdr front-ptr) new-node)
               (set-car! (cdr new-node) front-ptr)
               (set! front-ptr new-node)))))
    
    (define (rear-insert-deque! item)
      (let ((new-node (make-node item '() '())))
        (cond ((empty-queue?)
               (set! front-ptr new-node)
               (set! rear-ptr new-node))
              (else
               (set-car! (cdr rear-ptr) new-node)
               (set-cdr! (cdr new-node) rear-ptr)
               (set! rear-ptr new-node)))))

    (define (front-delete-deque!)
      (cond ((empty-queue?)
             (error "DELETE! вызвана с пустой очередью"))
            (else
             (set! front-ptr (cadr front-ptr))
             (set-cdr! (cdr front-ptr) '()))))

    (define (rear-delete-deque!)
      (cond ((empty-queue?)
             (error "DELETE! вызвана с пустой очередью"))
            (else
             (set! rear-ptr (cddr rear-ptr))
             (set-car! (cdr rear-ptr) '()))))
      
    (define (dispatch m)
      (cond ((eq? m 'empty-deque?) (empty-queue?))
            ((eq? m 'front-deque) '())
            ((eq? m 'rear-deque) '())
            ((eq? m 'front-insert-deque!) front-insert-deque!)
            ((eq? m 'rear-insert-deque!) rear-insert-deque!)        
            ((eq? m 'front-delete-deque!) (front-delete-deque!))
            ((eq? m 'rear-delete-deque!) (rear-delete-deque!))
            ((eq? m 'print-deque) (print-deque))
            (else (error "Неизвестная процедура"))))
    dispatch))

(define q1 (make-deque))
(q1 'empty-deque?)
((q1 'front-insert-deque!) 'a)
((q1 'front-insert-deque!) 'b)
((q1 'front-insert-deque!) 'c)
(q1 'print-deque)
((q1 'rear-insert-deque!) 'd)
((q1 'rear-insert-deque!) 'e)
(q1 'print-deque)
"========="
(q1 'front-delete-deque!)
(q1 'rear-delete-deque!)
(q1 'print-deque)
(q1 'empty-deque?)
(q1 'print-deque)


