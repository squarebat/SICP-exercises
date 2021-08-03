#lang sicp
(define (make-interval a b) (cons (min a b) (max a b)))

(define (lower-bound interval) (car interval))
(define (upper-bound interval) (cdr interval))

(define (add-interval intr1 intr2)
  (make-interval (+ (lower-bound intr1) (lower-bound intr2))
                 (+ (upper-bound intr1) (upper-bound intr2))))

(define (sub-interval intr1 intr2)
  (make-interval (- (lower-bound intr1) (upper-bound intr2))
                 (- (upper-bound intr1) (lower-bound intr2))))

(define (mul-interval intr1 intr2)
  (let ((i1 (* (lower-bound intr1) (lower-bound intr2)))
        (i2 (* (lower-bound intr1) (upper-bound intr2)))
        (i3 (* (upper-bound intr1) (lower-bound intr2)))
        (i4 (* (upper-bound intr1) (upper-bound intr2))))
    (make-interval (min i1 i2 i3 i4)
                   (max i1 i2 i3 i4))
  )
)

(define (div-interval intr1 intr2)
  (mul-interval intr1
                (make-interval (/ 1.0 (upper-bound intr2))
                               (/ 1.0 (lower-bound intr2))))
)

(define A (make-interval 2 6))
(define B (make-interval 2 6))
(define (print-interval intr)
  (newline)
  (display "[")
  (display (lower-bound intr))
  (display ",")
  (display (upper-bound intr))
  (display "]"))

;A/A should be idealy 1 (1 represents identity for intervals). Lem demonstrates this by inroducing same intervals on two sides of fraction
(print-interval (div-interval A A))
(print-interval (div-interval A B))

;check for parallel resistance formulas
(define (par1 r1 r2)
  (div-interval (mul-interval r1 r2)
                (add-interval r1 r2)))
(define (par2 r1 r2)
  (let ((one (make-interval 1 1)))
    (div-interval one
                  (add-interval (div-interval one r1)
                                (div-interval one r2))))
)

;This demonstrates Eva is right. Par1 gives wrong answer since same intervals are introduced on both sides
(print-interval (par1 A B))
(print-interval (par2 A B))