#lang sicp

(define (divides? divisor n)
  (= (remainder n divisor) 0)
)
(define (smallest-divisor n)
  (define (div-iter divisor)
    (cond ((< n (* divisor divisor)) n)
          ((divides? divisor n) divisor)
          (else (div-iter (+ divisor 1))))
  )
  (div-iter 2)
)
(define (prime? n)
  (= (smallest-divisor n) n)
)




;test
(smallest-divisor 199)
(smallest-divisor 1999)
(smallest-divisor 19999)