#lang sicp
(define (make-point x y) (cons x y))

(define (x-point pt) (car pt))
(define (y-point pt) (cdr pt))

(define (make-segment p1 p2) (cons p1 p2))

(define (start-segment seg) (car seg))
(define (end-segment seg) (cdr seg))

(define (average x y) (/ (+ x y) 2))
(define (mid-point seg)
  (let ((p1 (start-segment seg))
        (p2 (end-segment seg)))
    (make-point (average (x-point p1) (x-point p2)) (average (y-point p1) (y-point p2)))
  )
)

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

;test
(define p1 (make-point 1 0))
(define p2 (make-point 3 0))
(define seg (make-segment p1 p2))
(print-point (mid-point seg))
