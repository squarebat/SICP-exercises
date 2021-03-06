#lang sicp
(define (odd? x)
  (= (remainder x 2) 1)
)
(define (square x) (* x x))
(define (fast-expt n exp)
  (define (fast-expt-iter res n exp)
    (cond ((= exp 0) res)
          ((odd? exp) (fast-expt-iter (* res n) n (- exp 1)))
          (else (fast-expt-iter res (square n) (/ exp 2)))
    )
  )
  (fast-expt-iter 1 n exp)
)
;test
(fast-expt 2 11)
