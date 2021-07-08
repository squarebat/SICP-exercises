#lang sicp
(define (cycle? x)
  (let ((visited-pairs '()))
    (define (cycle-iter x)
      (cond ((null? x) #f)
            ((memq (car x) visited-pairs) #t)
            (else (begin
                    (set! visited-pairs (cons (car x) visited-pairs))
                    (cycle-iter (cdr x))))))
    (cycle-iter x)))

;test
(define list1 (list 1 2 3))
(cycle? list1)

(define list-with-cycle '(foo bar baz)) 
(set-cdr! (cddr list-with-cycle) list-with-cycle)
(cycle? list-with-cycle)

