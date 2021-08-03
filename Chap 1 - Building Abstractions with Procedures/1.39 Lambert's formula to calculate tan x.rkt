#lang sicp

(define (cont-frac n d k)
  (define (iter step result)
    (if (= step 0)
        (/ (n (+ step 1)) result)
        (iter (- step 1) (+ (d step) (/ (n (+ step 1)) result)))
    )
  )
  (iter (- k 1) (d k))
)

;compute tan x (x in radians)
(define (tan-cf x k)
  (cont-frac (lambda (i)
               (if (= i 1)
                   x
                   (* -1.0 x x))
             )
             (lambda (i)
               (+ 1.0 (* 2 (- i 1)))
             )
             k)
)

;test 
(define pi 3.141592653589793)
(tan-cf pi 100)
(tan-cf (/ pi 2) 100)
(tan-cf (/ pi 4) 100)