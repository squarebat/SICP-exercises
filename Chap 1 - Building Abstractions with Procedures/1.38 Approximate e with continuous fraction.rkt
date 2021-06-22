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

; lambda function d based on fact that a multiple of 2 occurs when i = 2,5,8,... etc.
; We find the position at which i occurs in the A.P above and multiply it with 2
(define (approx-e)
  (+ 2 (cont-frac (lambda (i) 1.0)
                  (lambda (i)
                    (if (or (= i 2)
                            (= (remainder (- i 2) 3) 0))
                        (* 2 (+ 1 (/ (- i 2) 3)))
                        1.0
                        )
                    )
                  100))
)

(approx-e)