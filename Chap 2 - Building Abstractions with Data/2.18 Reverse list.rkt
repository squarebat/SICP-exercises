#lang sicp
(define (reverse items)
  (if (null? items)
      nil
      (append (reverse (cdr items)) (list (car items)))))
(reverse (list 1 2 3 4))