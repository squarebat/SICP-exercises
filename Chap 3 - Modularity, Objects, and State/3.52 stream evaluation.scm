#lang sicp
;---- METHODS FOR TEST ----
(define (stream-car stream) (car stream))
(define (stream-cdr stream) (force (cdr stream)))

(define (stream-enumerate-interval low high)
  (if (> low high)
      the-empty-stream
      (cons-stream
       low
       (stream-enumerate-interval (+ low 1) high))))

(define (stream-map proc s)
  (if (stream-null? s)
      the-empty-stream
      (cons-stream (proc (stream-car s))
                   (stream-map proc (stream-cdr s)))))

(define (stream-filter pred stream)
  (cond ((stream-null? stream) the-empty-stream)
        ((pred (stream-car stream))
         (cons-stream (stream-car stream)
                      (stream-filter pred
                                     (stream-cdr stream))))
        (else (stream-filter pred (stream-cdr stream)))))

(define (stream-ref s n)
  (if (= n 0)
      (stream-car s)
      (stream-ref (stream-cdr s) (- n 1))))

(define (display-stream s)
  (stream-for-each display-line s))

(define (display-line x)
  (newline)
  (display x))

(define (stream-for-each proc s)
  (if (stream-null? s)
      'done
      (begin (proc (stream-car s))
             (stream-for-each proc (stream-cdr s)))))
;---- FUN STARTS HERE ----
(define sum 0)
sum
;sum = 0

(define (accum x)
  (set! sum (+ x sum))
  sum)

(define seq (stream-map accum (stream-enumerate-interval 1 20)))
sum
;enumerate returns a pair of number 1 and promise to evaluate (stream-enumerate-interval 2 20)
;accum takes the first element 1, sets sum to 1 and returns sum.
;seq is a pair of number 1 and a promise to evaluate stream-map on the rest of enumerated stream
;sum = 1
(define y (stream-filter even? seq))
sum
;this call forces evaluation of stream-cdr until even element occurs
;this sets sum to 1+2 = 3, 3 is not even, so execution continues
;sum is set to 3+3 = 6 in the next call
;sum is set to 6
;y is now a pair of 6 and a promise to evaluate and filter the rest of seq
(define z (stream-filter (lambda (x) (= (remainder x 5) 0))
                         seq))
sum
;in previous call we evaluated seq up until (1 3 6 promise)
;above call evaluates seq till a number divisible by 5 occurs
;hence seq evaluates till (1 3 6 10...)
;and set is set to pair of 10 and a promise
;sum = 10
(stream-ref y 7)
;this evaluates seq up till (1 3 6 10 15 21 28 36 45 55 66 78 91 105 120 136 promise) 
;sum = 136
(display-stream z)
sum
;display stream will evaluate the entire seq and sum will be set to 210