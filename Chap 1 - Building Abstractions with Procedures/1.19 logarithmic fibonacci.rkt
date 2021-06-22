#lang sicp
#|
p' <- p^2 + q^2
q' <- q^2 + 2pq
|#
(define (even? x) (= (remainder x 2) 0))
(define (square x) (* x x))
(define (sum_of_squares x y) (+ (square x) (square y)))
(define (fib n)
  (fib-iter 1 0 0 1 n)
)
(define (fib-iter a b p q count)
  (cond ((= count 0) b)
        ((even? count)
         (fib-iter a
                   b
                   (sum_of_squares p q)
                   (+ (square q) (* 2 p q))
                   (/ count 2)))
        (else (fib-iter (+ (* b q) (* a q) (* a p))
                        (+ (* b p) (* a q))
                        p
                        q
                        (- count 1))))
)

(fib 1)
(fib 2)
(fib 3)
(fib 4)
(fib 5)
(fib 6)
(fib 7)