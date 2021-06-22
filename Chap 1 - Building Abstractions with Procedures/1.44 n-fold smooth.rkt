#lang sicp

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

(define (deriv g x)
  (let ((dx 0.00001))
    (/ (- (g (+ x dx)) (g x)) dx)
  )
)

(define (smooth f)
  (lambda (x)
    (/
     (+
      (f (- x (deriv f x)))
      (f x)
      (f (+ x (deriv f x))))
     3)
  )
)

(define (n-fold-smooth f n)
  ((repeated smooth n) f))
