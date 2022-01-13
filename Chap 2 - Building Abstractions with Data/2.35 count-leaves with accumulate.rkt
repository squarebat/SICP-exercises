#lang sicp
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (count-leaves t)
  (accumulate
   +
   0
   (map (lambda (sub-tree)
         (cond
           ((null? sub-tree) 0)
           ((not (pair? sub-tree)) 1)
           (else (count-leaves sub-tree)))) t)))

(define tree (list nil (list 2 (list 3 4) (list 5)) 6 7 (list 8 9 10)))
(count-leaves tree)
