#lang sicp

(define tolerance 0.0001)
(define (fixed-point-with-log f first-guess)
  (define (close-enough? n1 n2)
    (< (abs (- n1 n2)) tolerance)
  )
  (define (try guess step)
    (let ((next (f guess)))
      (display step)
      (display ": ")
      (display next)
      (newline)
      (if (close-enough? guess next)
          next
          (try next (+ step 1))
      ))
  )
  (try first-guess 1)
)

(define (x-raise-to-x-equals value)
  (fixed-point-with-log (lambda (x) (/ (log value) (log x))) 2.0)
)

; test without average damp takes 30 steps
(display "Test without average damp:")
(newline)
(x-raise-to-x-equals 1000)

(newline)
(display "Test with average damp:")
(newline)

(define (average x y) (/ (+ x y) 2))
(define (average-damp f)
  (lambda (x) (average (f x) x)))

(define (x-raise-to-x-equals-avg-damp value)
  (define func (lambda (x) (/ (log value) (log x))))
  (fixed-point-with-log (average-damp func) 2.0)
)

(x-raise-to-x-equals-avg-damp 1000)