#lang sicp
(define (last-pair l)
  (if (null? (cdr l))
             (list (car l))
             (last-pair (cdr l))
  )
)

(last-pair (list 1 2 3 4))