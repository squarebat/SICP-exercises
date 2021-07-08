#lang scheme
(define (make-accumulator sum)
  (lambda (number)
    (set! sum (+ sum number))
    sum))
(define A (make-accumulator 5))
(A 5)
(A 5)
(A 3)