#lang sicp
(define (inc x) (+ x 1))

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))
  )
)
  
(define (simpson-integral f a b n)
  (define h (/ (- b a) n))
  (define (cal-term k)
    (define func-val (f (+ a (* k h))))
    (cond ((or (= k 0) (= k n)) func-val)
          ((= (remainder k 2) 1) (* 4 func-val))
          (else (* 2 func-val)))
  )
  (* (/ h 3) (sum cal-term 0 inc n))  
)
  
(define (cube x) (* x x x))
(simpson-integral cube 0 1.0 100)
(simpson-integral cube 0 1.0 1000)