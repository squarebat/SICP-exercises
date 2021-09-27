#lang sicp
;boring utility stuff
(define (stream-car stream) (car stream))
(define (stream-cdr stream) (force (cdr stream)))

(define (interleave s1 s2)
  (if (stream-null? s1)
      s2
      (cons-stream (stream-car s1)
                   (interleave s2 (stream-cdr s1)))))

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

(define (stream-map proc s)
  (if (stream-null? s)
      the-empty-stream
      (cons-stream (proc (stream-car s))
                   (stream-map proc (stream-cdr s)))))

(define (divisible? a b)
  (= (remainder a b) 0))

;--------Fun here----------
#|
weight is function that returns -
-1 if arg1 is less than arg2
0 if arg1 has the same weight as arg2
+1 if arg1 is greater than arg2
|#
(define (merge-weighted s1 s2 weight)
  (cond ((stream-null? s1) s2)
        ((stream-null? s2) s1)
        (else
         (let ((s1car (stream-car s1))
               (s2car (stream-car s2)))
           (cond ((= (weight s1car s2car) -1)
                  (cons-stream s1car (merge-weighted (stream-cdr s1) s2 weight)))
                 ((= (weight s1car s2car) 1)
                  (cons-stream s2car (merge-weighted s1 (stream-cdr s2) weight)))
                 (else
                  (cons-stream s1car
                               (merge-weighted
                                (stream-cdr s1)
                                (stream-cdr s2)
                                weight))))))))

(define (pairs-weighted s t weight)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (merge-weighted
    (stream-map (lambda (x) (list (stream-car s) x))
                (stream-cdr t))
    (pairs-weighted (stream-cdr s) (stream-cdr t) weight)
    weight)))

;a. the stream of all pairs of positive integers (i,j) with i < j ordered according to the sum i + j

(define (integer-pair-weight pair1 pair2)
  (let ((sum1 (+ (car pair1) (cadr pair1)))
        (sum2 (+ (car pair2) (cadr pair2))))
    (if (< sum1 sum2)
        -1
        +1)))

(define (integers-starting-from n)
  (cons-stream n (integers-starting-from (+ n 1))))
(define integers (integers-starting-from 1))

(define ordered-integer-pairs (pairs-weighted integers integers integer-pair-weight))

(stream-ref ordered-integer-pairs 0)
(stream-ref ordered-integer-pairs 1)
(stream-ref ordered-integer-pairs 2)
(stream-ref ordered-integer-pairs 3)
(stream-ref ordered-integer-pairs 4)

;b. the stream of all pairs of positive integers (i,j) with i < j, where neither i nor j is divisible by 2, 3, or 5, and the pairs are ordered according to the sum 2 i + 3 j + 5 i j. 

(define (non235-pair-weight pair1 pair2)
  (let ((sum1 (+ (* 2 (car pair1)) (* 3 (cadr pair1)) (* 5 (car pair1) (cadr pair1))))
        (sum2 (+ (* 2 (car pair2)) (* 3 (cadr pair2)) (* 5 (car pair2) (cadr pair2)))))
    (if (< sum1 sum2)
        -1
        +1)))

(define (non235-filter number)
  (if (or (divisible? number 2)
          (divisible? number 3)
          (divisible? number 5))
      false
      true))

(define non235-integers (stream-filter non235-filter integers))
(define ordered-non235-pairs
  (pairs-weighted non235-integers non235-integers non235-pair-weight))

(stream-ref ordered-non235-pairs 0)
(stream-ref ordered-non235-pairs 1)
(stream-ref ordered-non235-pairs 2)
(stream-ref ordered-non235-pairs 3)
(stream-ref ordered-non235-pairs 4)
