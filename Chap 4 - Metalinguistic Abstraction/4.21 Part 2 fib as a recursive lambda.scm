;fib can be defined as
((lambda (n)
   ((lambda (fib)
      (fib fib n))
    (lambda (fb k)
      (cond ((= k 1) 0)
            ((= k 2) 1)
            (else (+ (fb fb (- k 2)) (fb fb (- k 1))))))))
 8)
