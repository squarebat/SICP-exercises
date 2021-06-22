#lang sicp
(define (reverse items)
  (define (iter items result)
    (cond ((null? items) result)
          ((list? (car items)) (iter (cdr items) (cons (reverse (car items)) result)))
        (else (iter (cdr items) (cons (car items) result)))))
  (iter items nil))

(reverse (list 1 2 (list 3 4 5) 6 (list 7 8 9 10 (list 11 12 (list 13 14 15) 16 (list 17 18 19 20)))))