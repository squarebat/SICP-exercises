#lang sicp
;define required arithmetic functions
(define (odd? x) (= (remainder x 2) 1))
(define (square x) (* x x))  
(define (pow n exp)
  (define (pow-iter res n exp)
    (cond ((= exp 0) res)
          ((odd? exp) (pow-iter (* res n) n (- exp 1)))
          (else (pow-iter res (square n) (/ exp 2)))
    )
  )
  (pow-iter 1 n exp)
)

;return log a to the base b
(define (log-base a b)
  (/ (log a) (log b)))

;returns the exponent of the factor 2 in an integer x
(define (factorize x)
  (define (iter x power2)
    (if (odd? x)
        power2
        (iter (/ x 2) (+ power2 1))))
  (iter x 0.0)
)

;representing pair a,b as 2^a*3^b
(define (cons a b)
  (* (pow 2 a) (pow 3 b)))

(define (car pair)
  (factorize pair))

(define (cdr pair)
  (log-base (/ pair (pow 2 (factorize pair))) 3)
)

(define pair (cons 3.0 4.0))
pair
(car pair)
(cdr pair)
