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
(define (square x) (* x x))
;test
((repeated square 2) 5)