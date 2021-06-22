#lang sicp
;vector representation
(define (make-vect x y) (cons x y))
(define (xcor-vect vect) (car vect))
(define (ycor-vect vect) (cdr vect))

(define (add-vect v1 v2) (make-vect (+ (xcor-vect v1) (xcor-vect v2)) (+ (ycor-vect v1) (ycor-vect v2))))
(define (sub-vect v1 v2) (make-vect (- (xcor-vect v1) (xcor-vect v2)) (- (ycor-vect v1) (ycor-vect v2))))
(define (scale-vect v scale) (make-vect (* scale (xcor-vect v)) (* scale (ycor-vect v))))

;frame rep
(define (make-frame origin edge1 edge2)
  (list origin edge1 edge2))

(define (origin-frame frame) (car frame))
(define (edge1-frame frame) (cadr frame))
(define (edge2-frame frame) (caddr frame))

(define (frame-coord-map frame)
  (lambda (v)
    (add-vect
     (origin-frame frame)
     (add-vect (scale-vect (xcor-vect v)
                           (edge1-frame frame))
               (scale-vect (ycor-vect v)
                           (edge2-frame frame))))))

;segment representation
(define (make-segment start-point end-point) (cons start-point end-point))
(define (start-segment seg) (car seg))
(define (end-segment seg) (cdr seg))
(define (get-mid-point seg)
  (define (avg2 a1 a2) (/ (+ a1 a2) 2))
  (let ((start-point (start-segment seg))
        (end-point (end-segment seg)))
    (make-vect (avg2 (xcor-vect start-point) (xcor-vect end-point))
               (avg2 (ycor-vect start-point) (ycor-vect end-point)))))

(define (segments->painter segment-list)
  (lambda (frame)
    (for-each
     (lambda (segment)
       (draw-line
        ((frame-coord-map frame) (start-segment segment))
        ((frame-coord-map frame) (end-segment segment))))
     segment-list)))

(define (fourth-corner frame)
  (let ((mid-point
         (get-mid-point (make-segment
                         (edge1-frame frame)
                         (edge2-frame frame)))))
    (make-vect (- (* 2 (xcor-vect mid-point) (xcor-vect (origin-frame frame))))
               (- (* 2 (ycor-vect mid-point) (ycor-vect (origin-frame frame)))))))

(define (outline-painter frame)
  (let ((frame-segments (list (make-segment (origin-frame frame) (edge1-frame frame))
                              (make-segment (origin-frame frame) (edge2-frame frame))
                              (make-segment (edge1-frame frame) (fourth-corner frame))
                              (make-segment (edge2-frame frame) (fourth-corner frame)))))
    (segments->painter frame-segments)))



