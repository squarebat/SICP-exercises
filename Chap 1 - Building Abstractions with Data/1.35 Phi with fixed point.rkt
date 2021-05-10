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

(define (compute-phi)
  (fixed-point (lambda (x) (+ 1 (/ 1 x))) 1.0)
)

(compute-phi)