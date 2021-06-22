#lang sicp
(define (filtered-accumulate filter combiner null-value term a next b)
  (if (> a b)
      null-value
      (begin
        (if (filter a)
            (combiner (term a)
                      (filtered-accumulate filter combiner null-value term (next a) next b))
            (combiner null-value
                      (filtered-accumulate filter combiner null-value term (next a) next b))            
        )
      )
  )
)

;sum of prime squares
(define (inc x) (+ x 1))
(define (square x) (* x x))

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

(define (sum-of-square-primes a b)
  (filtered-accumulate prime? + 0 square a inc b))

(display "Sum of prime squares in 1-6")
(newline)
(sum-of-square-primes 1 6)

;product of all positive integers co prime to n
(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))
  )
)

(define (return x) x)
(define (sum-coprime n)
  (define (coprime? a)
    (= (gcd a n) 1)
  )
  (filtered-accumulate coprime? + 0 return 1 inc n)
)

(display "Sum of all positive integers coprime to 10")
(newline)
(sum-coprime 10)