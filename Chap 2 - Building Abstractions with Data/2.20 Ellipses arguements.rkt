#lang sicp
(define (same-parity first . items)
  (define (parity x) (remainder x 2))
  (define (eq-parity? x y) (= (parity x) (parity y)))
  (define (iter items result)
    (cond ((null? items) (reverse result))
        ((eq-parity? first (car items))
         (iter (cdr items) (cons (car items) result)))
        (else (iter (cdr items) result)))
  )
  (iter items (list first))
)
  
(same-parity 1 2 3 4 5 6 7)
