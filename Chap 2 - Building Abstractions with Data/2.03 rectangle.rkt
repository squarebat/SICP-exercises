#lang sicp
(define (make-point x y) (cons x y))

(define (x-point pt) (car pt))
(define (y-point pt) (cdr pt))

;a rectangle can be represented by two points across a diagonal of the rectangle.
(define (make-rectangle p1 p2)
  (cons p1 p2))

(define (corner-1 rect)
  (car rect))

(define (corner-2 rect)
  (cdr rect))

(define (length rect)
  (abs (- (x-point (corner-1 rect)) (x-point (corner-2 rect)))))

(define (breadth rect)
  (abs (- (y-point (corner-1 rect)) (y-point (corner-2 rect)))))

#|since perimeter and area work with length and breadth of rectangle, the procedures will work regardless of change in representation,
as long as length and breath are correct.|#

(define (perimeter rect)
  (+ (* 2 (length rect))
     (* 2 (breadth rect))))

(define (area rect)
  (* (length rect)
     (breadth rect)))
