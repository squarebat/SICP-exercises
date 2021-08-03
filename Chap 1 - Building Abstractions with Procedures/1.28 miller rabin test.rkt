#lang sicp
(define (even? x)
  (= (remainder x 2) 0))

(define (square-check x mod)
  (define x-square (remainder (* x x) mod))
  (if (and (not (= x 1)) (not (= x (- mod 1))) (= x-square 1)) 0 x-square)
  )

(define (miller-rabin-expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp) (square-check (miller-rabin-expmod base (/ exp 2) m) m))
        (else (remainder (* base (miller-rabin-expmod base (- exp 1) m)) m)))  
)

(define (miller-rabin-test n)
  (define (try-it a)
    (= (miller-rabin-expmod a (- n 1) n) 1)
  )
  (try-it (+ 1 (random (- n 1))))
)

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((miller-rabin-test n) (fast-prime? n (- times 1)))
        (else false))
)

;test on carmichael numbers
;unfooled on multiple tests!
(fast-prime? 561 10)
(fast-prime? 1105 10)
(fast-prime? 1729 10)
(fast-prime? 2465 10)
(fast-prime? 2821 10)
(fast-prime? 6601 10)
(newline)
;test on prime numbers
(fast-prime? 11 10)
(fast-prime? 53 10)
(fast-prime? 97 10)
(newline)
;test on composite numbers
(fast-prime? 15 10)
(fast-prime? 35 10)
(fast-prime? 288 10)
