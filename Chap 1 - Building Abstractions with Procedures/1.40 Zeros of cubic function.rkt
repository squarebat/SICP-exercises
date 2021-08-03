#lang sicp

(define tolerance 0.000001)
(define (fixed-point f first-guess)
  (define (close-enough? n1 n2)
    (< (abs (- n1 n2)) tolerance)
  )
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next)
      ))
  )
  (try first-guess)
)

(define (square x) (* x x))
(define (cube x) (* x x x))
(define (cubic a b c)
  (lambda (x) (+ (cube x)
                 (* a (square x))
                 (* b x)
                 c))
)

(define (deriv g x)
  (let ((dx 0.00001))
    (/ (- (g (+ x dx)) (g x)) dx)
  )
)

(define (newtons-transform g)
  (lambda (x) (- x (/ (g x) (deriv g x))))
)

(define (newtons-method g guess)
  (fixed-point (newtons-transform g) guess)
)

;test
(newtons-method (cubic -1 -1 -1) 1.0)
;verify that return value is a Zero point
((lambda (x a b c) (+ (* x x x) (* a x x) (* b x) c)) 1.8392867552139966 -1 -1 -1)