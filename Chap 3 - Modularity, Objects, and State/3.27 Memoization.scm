#lang sicp
;memo-fib as (memoize fib) won't work since recursive calls to memofib will
;call fib as defined in the global environment and memoization will be lost
;Rather, we can assign fib to the memoized version of fib as -
;(set! fib (memoize fib))
;Now fib can be used in the same way as memo-fib
(define (fib n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (+ (fib (- n 1))
                 (fib (- n 2))))))

(define memo-fib
  (memoize (lambda (n)
             (cond ((= n 0) 0)
                   ((= n 1) 1)
                   (else (+ (memo-fib (- n 1))
                            (memo-fib (- n 2))))))))

(define (memoize f)
  (let ((table (make-table)))
    (lambda (x)
      (let ((previously-computed-result (lookup x table)))
        (or previously-computed-result
            (let ((result (f x)))
              (insert! x result table)
              result))))))