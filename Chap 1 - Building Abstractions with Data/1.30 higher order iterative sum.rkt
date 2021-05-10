#lang sicp

(define (inc x) (+ x 1))

(define (sum term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ result (term a)))
    )
  )
  (iter a 0)
)

;test on simple summation, square and cube
(define (return x) x)
(define (square x) (* x x))
(define (cube x) (* x x x))
(sum return 1 inc 10)
(sum square 1 inc 10)
(sum cube 1 inc 10)