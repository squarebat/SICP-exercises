#lang sicp
(define (make-interval a b) (cons a b))

(define (lower-bound interval) (min (car interval) (cdr interval)))
(define (upper-bound interval) (max (car interval) (cdr interval)))

(define (sub-interval intr1 intr2)
  (make-interval (- (lower-bound intr1) (upper-bound intr2))
                 (- (upper-bound intr1) (lower-bound intr2))))

