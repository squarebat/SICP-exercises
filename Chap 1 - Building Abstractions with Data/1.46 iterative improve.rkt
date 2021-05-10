#lang sicp

(define (iterative-improve good-enough? improve)
  (lambda (initial-guess)
    (define (try guess)
      (let ((next (improve guess)))
        (if (good-enough? guess next)
            next
            (try next)
            ))
    )
    (try initial-guess)  
  )
)

;sqrt implementation
(define (sqrt n)
  (define (average x y)
    (/ (+ x y) 2)
  )
  (define sqrt-iter (iterative-improve
   (lambda (guess new_guess)
     (< (/ (abs (- new_guess guess)) guess) 0.0001))
   (lambda (guess)
      (average  guess (/ n guess)))))
  (sqrt-iter 1.0))

;test
(sqrt 10000)

;fixed-point implementation
(define (fixed-point f initial-guess)
  (let ((tolerance 0.000001))
      (define (good-enough? n1 n2)
        (< (abs (- n1 n2)) tolerance)
      )
      ((iterative-improve good-enough? f) initial-guess)
  )
)

;test : approximate phi with fixed point
(fixed-point (lambda (x) (+ 1 (/ 1 x))) 1.0)