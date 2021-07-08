#lang sicp
;needed procedures
(define (square x) (* x x))

(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low (random range))))

(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond ((= trials-remaining 0)
           (/ trials-passed trials))
          ((experiment)
           (iter (- trials-remaining 1) (+ trials-passed 1)))
          (else
           (iter (- trials-remaining 1) trials-passed))))
  (iter trials 0))

;make-circle-predicate takes center coordinates and radius of circle
;returns a predicate proc that takes coordinates of a point
;and determines whether they're inside or outside the circle
(define (make-circle-predicate cx cy radius)
   (lambda (x y)
     (let ((distance (+ (square (- x cx)) (square (- y cy)))))
       (or (< distance (square radius))
           (= distance (square radius))))))

;estimate integral
(define (estimate-integral predicate x1 x2 y1 y2 trials)
  (define (in-circle-test)
    (predicate (random-in-range (min x1 x2) (max x1 x2))
               (random-in-range (min y1 y2) (max y1 y2))))
  
  (let ((area-rec (* (abs (- x1 x2)) (abs (- y1 y2)))))
   (* area-rec (monte-carlo trials in-circle-test))))

;test
(define circle-area (estimate-integral (make-circle-predicate 5.0 7.0 3.0) 2.0 8.0 4.0 10.0 100000))
circle-area
;estimate pi
(/ circle-area (square 3.0))