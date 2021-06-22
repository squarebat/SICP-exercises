#lang sicp
(define (even? x)
  (= (remainder x 2) 0))

(define (square x) (* x x))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp) (remainder (square (expmod base (/ exp 2) m)) m))
        (else (remainder (* base (expmod base (- exp 1) m)) m)))  
)

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a)
  )
  (try-it (+ 1 (random (- n 1))))
)

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else false))
)

(define (timed-prime-test n)
  (start-prime-test n (runtime))
)

(define (start-prime-test n start-time)
  (if (fast-prime? n 10)
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
