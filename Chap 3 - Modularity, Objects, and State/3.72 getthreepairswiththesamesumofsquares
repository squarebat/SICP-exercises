#lang sicp
;utility stuff
(define (stream-car stream) (car stream))
(define (stream-cdr stream) (force (cdr stream)))

(define (stream-ref s n)
  (if (= n 0)
      (stream-car s)
      (stream-ref (stream-cdr s) (- n 1))))

(define (stream-map proc s)
  (if (stream-null? s)
      the-empty-stream
      (cons-stream (proc (stream-car s))
                   (stream-map proc (stream-cdr s)))))

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

(define (square x)
  (* x x))
;-------------- FUN HERE --------------------
(define (cal-weight pair)
  (+ (square (car pair)) (square (cadr pair))))

(define (square-pair-weight pair1 pair2)
  (let ((sum1 (cal-weight pair1))
        (sum2 (cal-weight pair2)))
    (if (> sum1 sum2)
        +1
        -1)))
(define (integers-starting-from n)
  (cons-stream n (integers-starting-from (+ n 1))))
(define integers (integers-starting-from 1))

(define square-pairs (pairs-weighted integers integers square-pair-weight))

(define (get-square-triples pairs)
  (let ((weight1 (cal-weight (stream-ref pairs 0)))
        (weight2 (cal-weight (stream-ref pairs 1)))
        (weight3 (cal-weight (stream-ref pairs 2))))
    (if (= weight1 weight2 weight3)
        (cons-stream (list weight1 (stream-ref pairs 0) (stream-ref pairs 1) (stream-ref pairs 2))
                     (get-square-triples (stream-cdr (stream-cdr (stream-cdr pairs)))))
        (get-square-triples (stream-cdr pairs)))))

(define square-triples (get-square-triples square-pairs))
(stream-ref square-triples 0)
(stream-ref square-triples 1)
(stream-ref square-triples 2)
(stream-ref square-triples 3)
(stream-ref square-triples 4)
(stream-ref square-triples 5)
