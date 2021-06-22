#lang sicp
(define (f-recursive n)
  (if (< n 3)
      n
      (+
       (f-recursive (- n 1))
       (* 2 (f-recursive (- n 2)))
       (* 3 (f-recursive (- n 3))))
  )
)

(define (f-iterative n)
  (define (f-iter a b c count)
    (if (= count 0)
        c
        (f-iter (+ a (* 2 b) (* 3 c)) a b (- count 1))
        )
    )
  (f-iter 2 1 0 n)
)

(f-recursive 10)
(f-iterative 10)