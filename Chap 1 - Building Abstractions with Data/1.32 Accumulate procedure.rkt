#lang sicp
(define (accumulator-recursive combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a)
         (accumulator-recursive combiner null-value term (next a) next b))
  )
)

;sum and product using recursib=ve implmentation of accumulator
(define (sum term a next b)
  (accumulator-recursive + 0 term a next b))

(define (product term a next b)
  (accumulator-recursive * 1 term a next b))

(define (return x) x)
(define (inc x) (+ x 1))

;test
(display "Recursive sum and product test")
(newline)
(sum return 1 inc 10)
(product return 1 inc 10)

;iterative accumulator
(define (accumulator-iterative combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (combiner result (term a)))
    )
  )
  (iter a null-value)
)

;sum and product using iterative implementation of accumulator
(define (sum-iterative term a next b)
  (accumulator-iterative + 0 term a next b))

(define (product-iterative term a next b)
  (accumulator-iterative * 1 term a next b))

;test
(display "Iterative sum and product test")
(newline)
(sum-iterative return 1 inc 10)
(product-iterative return 1 inc 10)
