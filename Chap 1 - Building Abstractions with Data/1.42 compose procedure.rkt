#lang sicp
(define (compose f g)
  (lambda (x) (f (g x)))
)
;test
((compose (lambda (x) (* x x)) (lambda (x) (+ x 1))) 6)