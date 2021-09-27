#lang sicp
(define (stream-car stream) (car stream))
(define (stream-cdr stream) (force (cdr stream)))

(define (stream-map proc . argstreams)
  (if (stream-null? (car argstreams))
      the-empty-stream
      (cons-stream
       (apply proc (map stream-car argstreams))
       (apply stream-map
              (cons proc (map stream-cdr argstreams))))))

;test
(define s1 (cons-stream 1 (cons-stream 2 (cons-stream 3 '()))))
(define x (stream-map + s1 s1 s1))
(stream-car x)
(stream-car (stream-cdr x))
(stream-car (stream-cdr (stream-cdr x)))
