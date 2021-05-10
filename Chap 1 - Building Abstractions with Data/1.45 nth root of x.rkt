#lang sicp

(define (average x y) (/ (+ x y) 2))
(define (average-damp f)
  (lambda (x) (average (f x) x)))

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

(define (compose f g)
  (lambda (x) (f (g x)))
)
(define (repeated f n)
  (define (iter step repeat-func)
    (if (= step n)
        repeat-func
        (iter (+ step 1) (compose repeat-func f))
    )
  )
  (iter 1 f)
)

(define (pow n exp)
  (define (odd? x) (= (remainder x 2) 1))
  (define (square x) (* x x))
  (define (pow-iter res n exp)
    (cond ((= exp 0) res)
          ((odd? exp) (pow-iter (* res n) n (- exp 1)))
          (else (pow-iter res (square n) (/ exp 2)))
    )
  )
  (pow-iter 1 n exp)
)

(define (log2 x)
  (/ (log x) (log 2)))

(define (nth-root x n)
  (define nth-avg-damp (repeated average-damp (floor (log2 n))))
  (define x-by-power-y (lambda (y) (/ x (pow y (- n 1)))))
  (fixed-point
   (nth-avg-damp x-by-power-y)
   1.0)
)

;test
(nth-root 1024 10)
(nth-root 1024 5)