#lang sicp
#|
'-recursive' and '-iterative' has been used in to name procedures to specify the type of process used to implement a function.
Do not specify this while implementing procedures for purposes other than learning. A procedure name should not expose implementation details.
|#
(define (product-recursive term a next b)
  (if (> a b)
      1
      (* (term a)
         (product-recursive term (next a) next b))
  )
)

;factorial
(define (inc x) (+ x 1))
(define (return x) x)  
(define (factorial n)
  (product-recursive return 1 inc n)
)

(display "Factorial Test")
(newline)
(factorial 6)

;pi approximation
(define (square x) (* x x))
(define (approx-pi count)
  (define (pi-next a) (+ a 2))
  (define (pi-term a) (/ (* a (+ a 2)) (square (+ a 1))))
  (* 4 (product-recursive pi-term 2.0 pi-next count))
)

(display "Pi approximation Test")
(newline)
(approx-pi 100)
(approx-pi 1000)
(approx-pi 10000)

;iterative product procedure
(define (product-iterative term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (* result (term a)))
    )
  )
  (iter a 1)
)

(newline)
(display "Factorial Tests with Iterative product")
(newline)
(define (factorial-iterative n)
  (product-iterative return 1 inc n)
)

(factorial-iterative 4)
(factorial-iterative 5)
(factorial-iterative 6)
