#lang sicp
(define (cycle? lst)
  (define (cycle-iter ptr1 ptr2)
    (cond ((or (null? ptr1) (null? ptr2) (null? (cdr ptr2))) #f)
          ((eq? (car ptr1) (car ptr2)) #t)
          (else (cycle-iter (cdr ptr1) (cddr ptr2)))))
  (if (or (null? lst) (null? (cdr lst)))
      #f
      (cycle-iter (cdr lst) (cddr lst))))

;test
(define list1 (list 1 2 3))
(cycle? list1)

(define x '(foo))
(define y (cons x x))

(define list2 (list y)) 
(cycle? list2)

(define list3 (cons y y)) 
(cycle? list3)

(define list-with-cycle '(1 2 3 4 5)) 
(set-cdr! (cddr list-with-cycle) list-with-cycle)
(cycle? list-with-cycle)