#lang sicp
(define (averager a b c)
  (let ((sum (make-connector))
        (two (make-connector)))
    (adder a b sum)
    (multiplier c two sum)
    (constant 2 two)
    'ok))
