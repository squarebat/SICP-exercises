#lang sicp
(define (divides? divisor n)
  (= (remainder n divisor) 0)
)

(define (smallest-divisor n)
  (define (div-iter divisor)
    (cond ((< n (* divisor divisor)) n)
          ((divides? divisor n) divisor)
          ((= divisor 2) (div-iter 3))
          (else (div-iter (+ divisor 2))))
  )
  (div-iter 2)
)

(define (prime? n)
  (= (smallest-divisor n) n)
)

(define (timed-prime-test n)
  (start-prime-test n (runtime))
)

(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime (- (runtime) start-time) n))
)

(define (report-prime elapsed-time n)
  (newline)
  (display n)  
  (display " *** ")
  (display elapsed-time)
)

(define (search-for-primes left right)
  (define (iter left)
    (timed-prime-test left)
    (if (>= left right)
        (newline)
        (iter (+ left 2))))
  (iter left)
)

(search-for-primes 1001 1019)       ; 1e3 
(search-for-primes 10001 10037)     ; 1e4 
(search-for-primes 100001 100043)   ; 1e5 
(search-for-primes 1000001 1000037)

(search-for-primes 1000000001 1000000021)       ; 1e9 
(search-for-primes 10000000001 10000000061)     ; 1e10 
(search-for-primes 100000000001 100000000057)   ; 1e11 
(search-for-primes 1000000000001 1000000000063) ; 1e12 