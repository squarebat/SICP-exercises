#lang sicp
;mul streams will use the generalized definition of stream-map from ex 3.50
(define (stream-car stream) (car stream))
(define (stream-cdr stream) (force (cdr stream)))

(define (stream-map proc . argstreams)
  (if (stream-null? (car argstreams))
      the-empty-stream
      (cons-stream
       (apply proc (map stream-car argstreams))
       (apply stream-map
              (cons proc (map stream-cdr argstreams))))))

;mul-streams with the help of stream-map
(define (mul-streams s1 s2)
  (stream-map * s1 s2))

;integers stream
(define ones (cons-stream 1 ones))
(define (add-streams s1 s2)
  (stream-map + s1 s2))
(define integers (cons-stream 1 (add-streams ones integers)))

;creating a stream of factorials
(define factorials (cons-stream 1 (mul-streams factorials integers)))
