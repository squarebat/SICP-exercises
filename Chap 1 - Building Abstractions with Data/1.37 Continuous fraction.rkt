#lang sicp
;note that even though iter has been used, this procedure generates a recursive process as values will be stacked up in the then expression of if  
(define (cont-frac-recursive n d k)
  (define (iter step)
    (if (= step k)
        (/ (n step) (d step))
        (/ (n step) (+ (d step) (iter (+ step 1))))
    )
  )
  (iter 1)
)

;iterative version
(define (cont-frac-iterative n d k)
  (define (iter step result)
    (if (= step 0)
        (/ (n (+ step 1)) result)
        (iter (- step 1) (+ (d step) (/ (n (+ step 1)) result)))
    )
  )
  (iter (- k 1) (d k))
)

;compute 1/phi
;11 steps required for accuracy of 4 decimal places 
(cont-frac-recursive (lambda (i) 1.0)
                     (lambda (i) 1.0)
                     11)

(cont-frac-iterative (lambda (i) 1.0)
                     (lambda (i) 1.0)
                     11)