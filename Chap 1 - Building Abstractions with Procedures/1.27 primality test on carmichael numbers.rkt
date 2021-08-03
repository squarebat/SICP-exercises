#lang sicp

(define (exhaustive-fermat-test n)
  (define (fermat-test a)
    (= (expmod a n n) a)
  )
  (define (iter a)
    (cond ((= a 0) true)
          ((fermat-test a) (iter (- a 1)))
          (else false)
    )
  )
  (iter (- n 1))
)

(define (even? x)
  (= (remainder x 2) 0))

(define (square x) (* x x))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp) (remainder (square (expmod base (/ exp 2) m)) m))
        (else (remainder (* base (expmod base (- exp 1) m)) m)))  
)

;test on carmichael numbers
(exhaustive-fermat-test 561)
(exhaustive-fermat-test 1105)
(exhaustive-fermat-test 1729)
(exhaustive-fermat-test 2465)
(exhaustive-fermat-test 2821)
(exhaustive-fermat-test 6601)
(newline)
;test on prime numbers
(exhaustive-fermat-test 11)
(exhaustive-fermat-test 53)
(exhaustive-fermat-test 97)
(newline)
;test on composite numbers
(exhaustive-fermat-test 15)
(exhaustive-fermat-test 35)
(exhaustive-fermat-test 288)