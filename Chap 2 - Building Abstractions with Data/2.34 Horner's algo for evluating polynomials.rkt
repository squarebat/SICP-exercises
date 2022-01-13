#lang sicp
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (horner-eval value polynomial-coeffs)
  (accumulate (lambda (current-coeff accum-val) (+ current-coeff (* value accum-val))) 0 polynomial-coeffs))

(horner-eval 2 (list 1 3 0 5 0 1))